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
	InfoURL = "https://hex.pm/api/packages/$(1)",
	{ok, {{_, 200, _}, _, Body}} = httpc:request(get, 
		{InfoURL, [{"Accept", "application/vnd.hex+erlang"}]}, 
		[], [{body_format, binary}]),
	#{<<"releases">> := Releases} = binary_to_term(Body),
	Vsns = lists:map(fun(#{<<"version">> := Version}) -> 
			binary_to_list(Version) 
		end, Releases),
	Vsn = FunExpected(Vsns, "$(2)", FunMaxVersion),
	PackageURL = "https://s3.amazonaws.com/s3.hex.pm/tarballs/$(1)-" ++ Vsn ++ ".tar",
	{ok, {{_, 200, _}, _, PkgBody}} = httpc:request(get,
		{PackageURL, []},
		[], [{body_format, binary}]),
	{ok, Files} = erl_tar:extract({binary, PkgBody}, [memory]),
	{_, Source} = lists:keyfind("contents.tar.gz", 1, Files),
	ok = erl_tar:extract({binary, Source}, [{cwd, "$(call core_native_path,$(DEPS_DIR)/$1)"}, compressed]),
	halt().
endef

define dep_fetch_hexpm
	$(call erlang,$(call dep_fetch_hexpm.erl,$(1),$(strip $(wordlist 2,$(words $(dep_$(1))),$(dep_$(1))))));
endef

$(foreach dep,$(BUILD_DEPS) $(DEPS),$(eval $(call dep_target,$(dep))))

