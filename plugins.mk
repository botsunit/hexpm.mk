HEX_URL ?= https://repo.hex.pm
HEX_REGISTRY ?= registry.ets.gz
HEX_PATH ?= $(HOME)/.hex
HEX_REGISTRY_FILE = $(HEX_PATH)/$(HEX_REGISTRY)
HEX_REGISTRY_URL = $(HEX_URL)/$(HEX_REGISTRY)

ifeq ($(shell which gunzip 2>/dev/null | wc -l), 1)
define hex_gunzip
	gunzip -c $(2) > $(1)
endef
else
define hex_gunzip.erl
	{ok, Data} = file:read_file("$(1)"),
	Unzipped = zlib:gunzip(Data),
	ok = file:write_file("$(2)", Unzipped),
	halt().
endef

define hex_gunzip
	$(call erlang,$(call hex_gunzip.erl,$(call core_native_path,$1),$(call core_native_path,$2)))
endef
endif

hex-update: hex-clean $(basename $(HEX_REGISTRY_FILE))

hex-clean:
	$(verbose) rm -f $(HEX_REGISTRY_FILE)
	$(verbose) rm -f $(basename $(HEX_REGISTRY_FILE))

define hex_search.erl
	{ok, R} = ets:file2tab("$(basename $(HEX_REGISTRY_FILE))"),
	case ets:lookup(R, <<"$(1)">>) of
		[{_,[Releases]}] ->
			Vsns = lists:map(fun erlang:binary_to_list/1, Releases),
			io:format("~s: ~p~n", ["$(1)", Vsns]);
		_ ->
		  io:format("~n!! Package $(1) not found. Try to update the package index.~n")
	end,
	halt().
endef

define hex_search
	$(verbose) $(call erlang,$(call hex_search.erl,$(1)))
endef

ifdef p
hex-search: $(basename $(HEX_REGISTRY_FILE))
	$(call hex_search,$(p))
else
hex-search:
	$(verbose) printf "%s\n" "" \
		"ERR Missing package name" 
endif


$(basename $(HEX_REGISTRY_FILE)):
	$(verbose) mkdir -p $(HEX_PATH)
	$(verbose) mkdir -p $(HEX_PATH)/packages
	$(verbose) $(call core_http_get,$(HEX_REGISTRY_FILE),$(HEX_REGISTRY_URL))
	$(gen_verbose) $(call hex_gunzip,$(basename $(HEX_REGISTRY_FILE)),$(HEX_REGISTRY_FILE))

define dep_fetch_hexpm.erl
	FunToInteger = fun
			([]) -> nil;
			(X) -> list_to_integer(X)
		end,
	FunPrefixToAtom = fun
			("") -> pre;
			(P) -> list_to_atom(P)
		end,
	FunBuildPre = fun(Prefix, Value, Sep) ->
			Pre = FunPrefixToAtom(Prefix),
			case lists:member(Pre, [alpha, a, beta, b, pre, rc]) of
				true -> {Pre, Value};
				false -> {pre, Prefix ++ Sep ++ Value}
			end
		end,
	FunParsePre = fun
			([]) ->
				nil;
			(Pre) ->
				Re = "(?<pre>-[a-z]+)?(?<sep>[\\.|-])?(?<num>[0-9A-Za-z-\\.]+)?",
				case re:run(Pre, Re, [{capture, [pre, sep, num], list}]) of
					{match, [[\$$$$-|Prefix], Sep, Value]} -> FunBuildPre(Prefix, Value, Sep);
					{match, [[], _, Value]} -> {pre, Value};
					nomatch -> {pre, Pre}
				end
		end,
	FunParseBuild = fun
			([]) -> nil;
			([\$$$$+|Rest]) -> Rest
		end,
	FunParse = fun(Version) ->
			Version1 = string:strip(Version, both, 32),
			Re = "^v?(?<version>(?<major>\\\\d+)\\.?(?<minor>\\\\d+)?\\.?(?<patchlevel>\\\\d+)?)(?<pre>-[0-9A-Za-z-\\.]+)?(?<build>\\\\+[0-9A-Za-z-\\.]+)?\\\$$$$",
			case re:run(Version1, Re, [{capture, [major, minor, patchlevel, version, pre, build], list}]) of
				{match, [X0, Y0, Z0, V, Pre, Build]} ->
					{ok, #{major => FunToInteger(X0),
								 minor => FunToInteger(Y0),
								 patch => FunToInteger(Z0),
								 d => length([E||E<-[X0, Y0, Z0], E =/= ""]),
								 v => V,
								 pre => FunParsePre(Pre),
								 build => FunParseBuild(Build)}};
				nomatch ->
					{error, invalid_version}
			end
		end,
	FunTryToInteger = fun(V, E) ->
			V1 = try list_to_integer(V) 
				catch
					_:_ -> V
				end,
			E1 = try list_to_integer(E)
				catch
					_:_ -> E
				end,
			if
				is_integer(V1) andalso is_integer(E1) -> {V1, E1};
				true -> {V, E}
			end
		end,
	FunCompareV = fun
			(nil, nil) -> 0;
			(nil, _) -> -1;
			(_, nil) -> 1;
			(V, E) ->
				{V1, E1} = FunTryToInteger(V, E),
				if 
					V1 > E1 -> 1;
					V1 < E1 -> -1;
					true -> 0
				end
		end,
	FunCompareP = fun
			(nil, nil) -> 0;
			(nil, _) -> 1;
			({P, V}, {P, E}) ->
				FunCompareV(V, E);
			({a, V}, {alpha, E}) ->
				FunCompareV(V, E);
			({alpha, V}, {a, E}) ->
				FunCompareV(V, E);
			({b, V}, {beta, E}) ->
				FunCompareV(V, E);
			({beta, V}, {b, E}) ->
				FunCompareV(V, E);
			({b, _}, {a, _}) -> 1;
			({b, _}, {alpha, _}) -> 1;
			({beta, _}, {a, _}) -> 1;
			({beta, _}, {alpha, _}) -> 1;
			({pre, _}, {a, _}) -> 1;
			({pre, _}, {alpha, _}) -> 1;
			({pre, _}, {b, _}) -> 1;
			({pre, _}, {beta, _}) -> 1;
			({rc, _}, {a, _}) -> 1;
			({rc, _}, {alpha, _}) -> 1;
			({rc, _}, {b, _}) -> 1;
			({rc, _}, {beta, _}) -> 1;
			({rc, _}, {pre, _}) -> 1;
			(_, _) -> -1
		end,
	FunCompare = fun(Version, Expected) ->
			case FunParse(Version) of
				{ok, #{v := VVersion, 
							 pre := VPre, 
							 build := VBuild}} ->
					case FunParse(Expected) of
						{ok, #{v := EVersion, 
									 pre := EPre, 
									 build := EBuild}} ->
							case {FunCompareV(VVersion, EVersion), 
										FunCompareP(VPre, EPre), 
										FunCompareV(VBuild, EBuild)} of
								{0, 0, 0} -> 0;
								{1, _, _} -> 1;
								{0, 1, _} -> 1;
								{0, 0, 1} -> 1;
								_ -> -1
							end;
						_ -> {error, invalid_version_2}
					end;
				_ -> {error, invalid_version_1}
			end
		end,
	FunMaxVersion = fun(V1, V2) ->
			case FunCompare(V1, V2) of
				-1 -> V2;
				0 -> V1;
				1 -> V1;
				E -> E
			end
		end,
	FunSup = fun(Version, Expected) ->
			FunCompare(Version, Expected) =:= 1
		end,
	FunInf = fun(Version, Expected) ->
			FunCompare(Version, Expected) =:= -1
		end,
	FunEqual = fun(Version, Expected) ->
			FunCompare(Version, Expected) =:= 0
		end,
	FunBump = fun(Type, Version) ->
			case FunParse(Version) of
				{ok, #{major := Major,
							 minor := Minor,
							 patch := Patch,
							 v := V,
							 pre := Pre, 
							 build := _, 
							 d := _}} ->
					case Type of
						major ->
							{ok, lists:flatten(io_lib:format("~w.0.0", [Major + 1]))};
						minor ->
							{ok, lists:flatten(io_lib:format("~w.~w.0", [Major, Minor + 1]))};
						_ ->
							case Pre of
								nil -> 
									{ok, lists:flatten(io_lib:format("~w.~w.~w", [Major, Minor, Patch + 1]))};
								_ ->
									{ok, V}
							end
					end;
				E -> E
			end
		end,
	FunTild = fun(Version, Expected) ->
			case FunCompare(Version, Expected) of
				0 -> true;
				-1 -> false;
				1 -> 
					{ok, #{d := D}} = FunParse(Expected),
					{ok, MaxExpected} = if
																D =:= 3 ->
																	FunBump(minor, Expected);
																true ->
																	FunBump(major, Expected)
															end,
					case FunCompare(Version, MaxExpected) of
						-1 -> true;
						_ -> false
					end
			end
		end,
	FunMatch = fun(V, E) ->
				Fun = fun
						(_, Version, [\$$$$>, \$$$$=|Expected]) ->
							FunSup(Version, Expected) orelse FunEqual(Version, Expected);
						(_, Version, [\$$$$<, \$$$$=|Expected]) ->
							FunInf(Version, Expected) orelse FunEqual(Version, Expected);
						(_, Version, [\$$$$=, \$$$$=|Expected]) ->
							FunEqual(Version, Expected);
						(_, Version, [\$$$$>|Expected]) ->
							FunSup(Version, Expected);
						(_, Version, [\$$$$<|Expected]) ->
							FunInf(Version, Expected);
						(_, Version, [\$$$$~, \$$$$>|Expected]) ->
							FunTild(Version, Expected);
						(F, Version, [32|Expected]) ->
							F(F, Version, Expected);
						(_, Version, Expected) ->
							FunEqual(Version, Expected)
					end,
				Fun(Fun, V, E)
		end,
	FunExpected = fun(Versions, Expected, Fun) ->
				lists:foldl(fun(V, M) ->
						case {M, FunMatch(V, Expected)} of
							{nil, true} -> V;
							{_, true} -> Fun(V, M);
							_ -> M
						end
					end, nil, Versions)
		end,
	ssl:start(),
	inets:start(),
	case filelib:is_file("$(HEX_REGISTRY_FILE)") of
		true -> ok;
		false ->
			ok = filelib:ensure_dir("$(HEX_REGISTRY_FILE)"),
				{ok, {{_, 200, _}, _, IdxBody}} = httpc:request(get, {"$(HEX_REGISTRY_URL)", []}, 
				                                                [], [{body_format, binary}]),
				ok = file:write_file("$(HEX_REGISTRY_FILE)", IdxBody),
				{ok, Data} = file:read_file("$(HEX_REGISTRY_FILE)"),
				Unzipped = zlib:gunzip(Data),
				ok = file:write_file("$(basename $(HEX_REGISTRY_FILE))", Unzipped)
	end,
	{ok, R} = ets:file2tab("$(basename $(HEX_REGISTRY_FILE))"),
	[{_,[Releases]}] = ets:lookup(R, <<"$(1)">>),
	Vsns = lists:map(fun erlang:binary_to_list/1, Releases),
	Vsn = FunExpected(Vsns, "$(2)", FunMaxVersion),
	PackageFile = "$(HEX_PATH)/packages/$(1)-" ++ Vsn ++ ".tar",
	ok = filelib:ensure_dir(PackageFile),
	case filelib:is_file(PackageFile) of
		true -> ok;
		false ->
			PackageURL = "$(HEX_URL)/tarballs/$(1)-" ++ Vsn ++ ".tar",
			{ok, {{_, 200, _}, _, PkgBody}} = httpc:request(get, {PackageURL, []}, [], [{body_format, binary}]),
			ok = file:write_file(PackageFile, PkgBody)
	end,
	{ok, Files} = erl_tar:extract(PackageFile, [memory]),
	{_, Source} = lists:keyfind("contents.tar.gz", 1, Files),
	ok = erl_tar:extract({binary, Source}, [{cwd, "$(call core_native_path,$(DEPS_DIR)/$1)"}, compressed]),
	halt().
endef

define dep_fetch_hexpm
	$(call erlang,$(call dep_fetch_hexpm.erl,$(1),$(strip $(wordlist 2,$(words $(dep_$(1))),$(dep_$(1))))));
endef
define dep_fetch_hex
	$(call erlang,$(call dep_fetch_hexpm.erl,$(1),$(strip $(wordlist 2,$(words $(dep_$(1))),$(dep_$(1))))));
endef

define dep_target_for_hex
ifeq ($(word 1,$(dep_$(1))),hex)
$(call dep_target,$(1))
endif
ifeq ($(word 1,$(dep_$(1))),hexpm)
$(call dep_target,$(1))
endif
endef

$(foreach dep,$(BUILD_DEPS) $(DEPS),$(eval $(call dep_target_for_hex,$(dep))))

## Help

help::
	$(verbose) printf "%s\n" "" \
		"Hex.pm targets:" \
		"  hex-update           Updates the package index." \
		"  hex-search p=...     Search a package in the package index."

