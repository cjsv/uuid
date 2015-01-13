%% -*- mode: erlang; indent-tabs-mode: nil -*-
% Copyright (c) 2012-2015 Christopher Vance.
% Copyright (c) 2012-2013 Into Science Pty Ltd.
-module(uuid_server).
-author('Christopher Vance').

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2]). % gen_server required
-export([handle_info/2,terminate/2,code_change/3]). % gen_server required
%-export([format_status/2]). % gen_server optional
-export([start_link/0]). % gen_server api

%%% gen_server required callbacks

%% invoked by
%% gen_server:start_link, gen_server:start
%% (gen_server:init_it)
%%
%% @doc Standard gen_server callback. Initial state for the gen_server
%% server process.
%%
-spec init(Args :: term()) ->
                  {ok, State :: term()} |
                  {ok, State :: term(), timeout() | hibernate} |
                  {stop, Reason :: term()} |
                  ignore.
-record(state, {time_when, clock_seq, node}).
init([]) ->
    {ok, init_state()}.

%% invoked by
%% gen_server:call, gen_server:multi_call
%% (gen_server:handle_msg)
%%
%% @doc Standard gen_server callback. Handle synchronous requests.
%%
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply,
                          Reply :: term(),
                          NewState :: term(),
                          timeout() | hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop,
                          Reason :: term(),
                          Reply :: term(),
                          NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({time}, _From, State) ->
    time_uuid(State);                           % time based
handle_call({dce_security, user, User}, _From, State) ->
    dce_sec_uuid(user, User, State);            % time based
handle_call({dce_security, group, Group}, _From, State) ->
    dce_sec_uuid(group, Group, State);          % time based
handle_call({md5_hash, Name, Space}, _From, State) ->
    {ok, Reply} = md5_uuid(Name, Space),
    {reply, Reply, State};
handle_call({sha1_hash, Name, Space}, _From, State) ->
    {ok, Reply} = sha1_uuid(Name, Space),
    {reply, Reply, State};
handle_call({md5_hash, Name}, _From, State) ->
    {ok, Reply} = md5_uuid(Name),
    {reply, Reply, State};
handle_call({sha1_hash, Name}, _From, State) ->
    {ok, Reply} = sha1_uuid(Name),
    {reply, Reply, State};
handle_call({random}, _From, State) ->
    {ok, Reply} = random_uuid(),
    {reply, Reply, State};
handle_call({ncs}, _From, State) ->
    ncs_uuid(State);                            % time based
handle_call({split, UUID}, _From, State) ->
    {ok, Reply} = split_uuid(UUID),
    {reply, Reply, State};
handle_call({match, UUID, Name, Space}, _From, State) ->
    {ok, Reply} = match_uuid(UUID, Name, Space),
    {reply, Reply, State};
handle_call({age, UUID}, _From, State) ->
    {ok, Reply} = age_uuid(UUID),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% invoked by
%% gen_server:cast, gen_server:abcast
%% (gen_server:dispatch)
%%
%% @doc Standard gen_server callback. Handle a request not requiring a
%% reply.
%%
-spec handle_cast(Request :: term(),
                  State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% invoked by
%% (gen_server:dispatch)
%%
%% @doc Standard gen_server callback. Handle non-request information.
%%
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% invoked by
%% (gen_server:terminate)
%%
%% @doc Standard gen_server callback. Clean up State before stopping.
%%
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) ->
                       Ignored :: term().
terminate(_Reason, _State) ->
    ok.

%% invoked by
%% (gen_server:system_code_change)
%%
%% @doc Standard gen_server callback. Change State as a result of a code
%% change during release upgrade or downgrade.
%%
-spec code_change(OldVsn :: (term() | {down, term()}),
                  State :: term(),
                  Extra :: term()) ->
                         {ok, NewState :: term()} |
                         {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% gen_server optional callback

%% (removed)

%%% gen_server api

%%
%% @doc API suggested in OTP Design Principles User's Guide. Start
%% this gen_server process.
%%
-spec start_link() ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error,
                         Error :: {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% functions internal to uuid implementation

%% For RFC-4122 time-based UUIDs, we use 100 ns ticks from 1582-10-15.
-define(OFFSET1582, 16#1b21dd213814000).        % This value is in the RFC.
%% For old NCS UUIDs, we use 4 µs ticks from 1980-01-01.
-define(OFFSET1980, 3652).                      % 10 years including 2 leap.
-define(SECPERDAY, 24 * 60 * 60).               % Seconds per day.

%% We normally create RFC-4122 UUIDs, and use 2 high bits of
%% clk_seq_hi_res to say so, but those bits are overridden in
%% assemble/6.
-spec init_state() -> #state{}.
init_state() ->
    <<_:2, Seq:14>> = crypto:rand_bytes(2),
    #state{time_when=now(), clock_seq=Seq, node=get_mac()}.

%% Time for RFC-4122 UUIDs is 100ns ticks since 1582-10-15. For
%% time-based UUIDs, we use both time and MAC, so need to use and
%% update State.
-spec time_uuid(#state{}) -> {reply, integer(), #state{}}.
time_uuid(State) ->
    {State1, New} = new_time(State),
    <<THi:12, TMid:16, TLo:32>> = <<New:60>>,
    {reply,
     assemble(1, TLo, TMid, THi, State#state.clock_seq, State#state.node),
     State1}.

%% For DCE Security UUIDs, we sacrifice the bottom 32 bits of time
%% (slightly less that 10 minutes) to represent the POSIX user or
%% group, and the bottom 8 bits of clk_seq to indicate which of these
%% it is. For these UUIDs, we use both time and MAC, so need to use
%% and update State. (NCS on Apollo Domain/OS included host information
%% in its user IDs, so 32 bits was not a new idea.)
-spec dce_sec_uuid(user | group, integer(), #state{}) ->
                          {reply, integer(), #state{}}.
dce_sec_uuid(user, User, State) ->
    {State1, New} = new_time(State),
    <<THi:12, TMid:16, _:32>> = <<New:60>>,
    Seq = State#state.clock_seq,
    <<SHi:6, _:8>> = <<Seq:14>>,
    <<SeqSec:14>> = <<SHi:6, 0:8>>,
    {reply,
     assemble(2, User, TMid, THi, SeqSec, State#state.node),
     State1};
dce_sec_uuid(group, Group, State) ->
    {State1, New} = new_time(State),
    <<THi:12, TMid:16, _:32>> = <<New:60>>,
    Seq = State#state.clock_seq,
    <<SHi:6, _:8>> = <<Seq:14>>,
    <<SeqSec:14>> = <<SHi:6, 1:8>>,
    {reply,
     assemble(2, Group, TMid, THi, SeqSec, State#state.node),
     State1}.

%% For namespace UUIDs, we do not use time or MAC, so we don't
%% actually need to use or modify State.
-spec md5_uuid(string(), dns | url | oid | x500dn) -> {ok, integer()}.
md5_uuid(Name, Space) ->
    {ok, Space1} = space_uuid(Space),
    %Space2 = big_endian(Space1),
    Ctx = crypto:hash_init(md5),
    Ctx1 = crypto:hash_update(Ctx, <<Space1:128>>),
    Ctx2 = crypto:hash_update(Ctx1, Name),
    Hash = crypto:hash_final(Ctx2),
    <<TLo:32, TMid:16, VTHi:16, RSeq:16, Node:48, _/binary>>
        = Hash, %big_endian(Hash),
    {ok, assemble(3, TLo, TMid, VTHi, RSeq, Node)}.
-spec sha1_uuid(string(), dns | url | oid | x500dn) -> {ok, integer()}.
sha1_uuid(Name, Space) ->
    {ok, Space1} = space_uuid(Space),
    <<TLo:32, TMid:16, VTHi:16, RSeq:16, Node:48>> = big_endian(crypto:hash(sha, [<<Space1:128>>, Name])),
    {ok, assemble(5, TLo, TMid, VTHi, RSeq, Node)}.
%% The RFC describes the use of names only with namespaces, but we
%% also allow generation of UUIDs without specififying a namespace.
-spec md5_uuid(string()) -> {ok, integer()}.
md5_uuid(Name) when is_list(Name) ->
    <<TLo:32, TMid:16, VTHi:16, RSeq:16, Node:48>> = crypto:hash(md5, [Name]),
    {ok, assemble(3, TLo, TMid, VTHi, RSeq, Node)}.
-spec sha1_uuid(string()) -> {ok, integer()}.
sha1_uuid(Name) when is_list(Name) ->
    <<TLo:32, TMid:16, VTHi:16, RSeq:16, Node:48, _/binary>> =
        crypto:hash(sha, [Name]),
    {ok, assemble(5, TLo, TMid, VTHi, RSeq, Node)}.

%% For random UUIDs, we do not use time or MAC, so we don't actually
%% need to use or modify State.
-spec random_uuid() -> {ok, integer()}.
random_uuid() ->
    <<TLo:32, TMid:16, VTHi:16, RSeq:16, Node:48>> = crypto:rand_bytes(16),
    {ok, assemble(4, TLo, TMid, VTHi, RSeq, Node)}.

%% For NCS UUIDs, a different time representation, granularity, and
%% epoch is used. Given that we don't actually expect these to be
%% generated for real usage, this implementation is mostly of
%% historical value.
-spec ncs_uuid(#state{}) -> {reply, integer(), #state{}}.
ncs_uuid(State) ->
    {State1, New} = new_time_ncs(State),
    Node = State#state.node,
    <<Result:128>> = <<New:48, 0:16, 13:8, 0:8, Node:48>>,
    {reply, Result, State1}.

%% Splitting depends on the variant and version.
%% Where Time was theoretically derived from a clock, indicate what that was.
-spec split_uuid(binary() | integer()) -> {ok, list()}.
split_uuid(<<UUID:128>>) ->
    <<TLo:32, TMid:16, VTHi:16, RSeq:16, Node:48>> = <<UUID:128>>,
    <<Bits64:64, _:2, Bits62:62>> = <<UUID:128>>,
    <<_:1, Bits61:61>> = <<Bits62:62>>,
    <<Var0:1, Var1:1, Var2:1, _:13>> = <<RSeq:16>>,
    <<Ver:4, THi:12>> = <<VTHi:16>>,
    <<_:2, Seq:14>> = <<RSeq:16>>,
    <<SHi:6, SLo:8>> = <<Seq:14>>,
    <<_:1, Fam:7, Node7:56>> = <<RSeq:16, Node:48>>,
    <<Time6:48>> = <<TLo:32, TMid:16>>,
    <<Time3:60>> = <<THi:12, TMid:16, TLo:32>>,
    <<Time3HM:60>> = <<THi:12, TMid:16, 0:32>>,
    <<Time3FF:60>> = <<THi:12, TMid:16, 16#ffffffff:32>>,
    Result =
        case Var0 of
            0 ->
                [ncs,
                 {time, h(6, Time6)},
                 {reserved, VTHi},
                 {created, ncs_time(Time6)},
                 {node, h(1, Fam), h(7, Node7)}];
            1 ->
                case Var1 of
                    0 ->
                        case Ver of
                            1 ->
                                [{standard, time},
                                 {time, h(8, Time3)},
                                 {created, dce_time(Time3)},
                                 {seq, h(2, Seq)},
                                 {node, h(6, Node)}];
                            2 ->
                                case SLo of
                                    0 ->
                                        [{standard, dce, user},
                                         {user, h(4, TLo)},
                                         {time, h(8, Time3HM)},
                                         {created, dce_time(Time3HM),
                                          dce_time(Time3FF)},
                                         {seq, h(1, SHi)},
                                         {node, h(6, Node)}];
                                    1 ->
                                        [{standard, dce, group},
                                         {group, h(4, TLo)},
                                         {time, h(8, Time3HM)},
                                         {created, dce_time(Time3HM),
                                          dce_time(Time3FF)},
                                         {seq, h(1, SHi)},
                                         {node, h(6, Node)}];
                                    _ ->
                                        [{standard, dce, unknown, SLo, TLo},
                                         {time, h(8, Time3HM)},
                                         {created, dce_time(Time3HM),
                                          dce_time(Time3FF)},
                                         {seq, h(1, SHi)},
                                         {node, h(6, Node)}]
                                end;
                            3 ->
                                [{standard, md5, h(6, Time6), h(2, THi),
                                  h(8, Bits62)}];
                            4 ->
                                [{standard, random, h(6, Time6), h(2, THi),
                                  h(8, Bits62)}];
                            5 ->
                                [{standard, sha, h(6, Time6), h(2, THi),
                                  h(8, Bits62)}];
                            _ ->
                                [{standard, ver, Ver, unknown, h(6, Time6),
                                  h(2, THi), h(8, Bits62)}]
                        end;
                    1 ->
                        case Var2 of
                            0 ->
                                [{microsoft, h(8, Bits64), h(8, Bits61)}];
                            1 ->
                                [{future, h(8, Bits64), h(8, Bits61)}]
                        end
                end
        end,
    {ok, Result};
split_uuid(UUID) when is_integer(UUID) ->
    split_uuid(<<UUID:128>>).

-spec age_uuid(integer()) -> {ok, integer()} | undefined.
age_uuid(UUID) ->
    {ok, Split} = split_uuid(UUID),
    Num =
        case Split of
            [{standard, time}, {time, Str} | _] ->
                erlang:list_to_integer(Str, 16);
            [{standard, dce, _}, _, {time, Str} | _] ->
                erlang:list_to_integer(Str, 16);
            _ ->
                undefined
        end,
    case Num of
        undefined ->
            undefined;
        Num when is_integer(Num) ->
            Tick70 = Num - ?OFFSET1582,
            Sec70 = Tick70 div 10000000,
            {M, S, _} = now(),
            Result = M * 1000000 + S - Sec70,
            {ok, Result}
    end.

%% Does a presented UUID match the specified name and namespace.
-spec match_uuid(integer(), string(), dns | url | oid | x500dn) ->
                        {ok, true | false}.
match_uuid(UUID, Name, Space) when is_integer(UUID) ->
    {ok, MD5} = md5_uuid(Name, Space),
    {ok, SHA1} = sha1_uuid(Name, Space),
    case UUID == MD5 of
        true ->
            {ok, true};
        _ ->
            case UUID == SHA1 of
                true ->
                    {ok, true};
                _ ->
                    {ok, false}
            end
    end.

%% These four namespace UUIDs are specified in
%% Internet RFC-4122
%% ITU-T recommendation X.667 (09/2004)
%% ISO/IEC 9834-8:2005
-spec space_uuid(dns | url | oid | x500dn) -> {ok, integer()}.
space_uuid(dns) ->
    {ok, 16#6ba7b8109dad11d180b400c04fd430c8};
space_uuid(url) ->
    {ok, 16#6ba7b8119dad11d180b400c04fd430c8};
space_uuid(oid) ->
    {ok, 16#6ba7b8129dad11d180b400c04fd430c8};
space_uuid(x500dn) ->
    {ok, 16#6ba7b8149dad11d180b400c04fd430c8}.

%% Return a value slightly larger than a specified value of now().
-spec inc_now({integer(), 0..999999, 0..999999}) ->
                     {integer(), 0..999999, 0..999999}.
inc_now({M, S, U}) ->
    case U >= 999999 of
        true ->
            case S >= 999999 of
                true ->
                    {M + 1, 0, 0};
                _ ->
                    {M, S + 1, 0}
            end;
        _ ->
            {M, S, U + 1}
    end.

%% Given State, update to a newer time_when.
-spec new_time_when(#state{}) -> #state{}.
new_time_when(State) ->
    Now = now(),
    case Now > State#state.time_when of
        true ->
            Now1 = Now;
        _ ->
            Now1 = inc_now(State#state.time_when)
    end,
    State#state{time_when=Now1}.

%% Given State, generate a newer RFC-4122 UUID timestamp, and update
%% time_when.
-spec new_time(#state{}) -> {#state{}, integer()}.
new_time(State) ->
    State1 = new_time_when(State),
    {M, S, U} = State1#state.time_when,
    New = ((((M * 1000000) + S) * 1000000 + U) * 10) +
        crypto:rand_uniform(0, 10) + ?OFFSET1582,
    {State1, New}.

%% Given State, generate a newer NCS UUID timestamp, and update
%% time_when.
-spec new_time_ncs(#state{}) -> {#state{}, integer()}.
new_time_ncs(State) ->
    State1 = new_time_when(State),
    {M, S, U} = State1#state.time_when,
    New = (((M * 1000000) + S) * 1000000 + U) div 4 -
        (?OFFSET1980 * ?SECPERDAY),
    {State1, New}.

%% Given a number of parts, assemble a UUID.
-spec assemble(1 .. 5, integer(), integer(), integer(), integer(), integer())
              ->
                      integer().
assemble(Ver, TLo, TMid, VTHi, RSeq, Node) ->
    <<_:4, THi:12>> = <<VTHi:16>>,
    <<_:2, Seq:14>> = <<RSeq:16>>,
    <<Result:128>> = <<TLo:32, TMid:16, Ver:4, THi:12, 2:2, Seq:14, Node:48>>,
    Result.

%% Get the Ethernet (or similar) MAC address from a network interface
%% on the host, or use a randomly generated (multicast) alternative.
-spec get_mac(list()) -> integer().
get_mac([{_, Stuff} | T]) ->
    Val = proplists:get_value(hwaddr, Stuff),
    case Val of
        undefined ->
            get_mac(T);
        [A, B, C, D, E, F] ->
            <<Have:48>> = <<A:8, B:8, C:8, D:8, E:8, F:8>>,
            Have
    end;   
get_mac([]) ->
    random_mac().
-spec get_mac() -> integer().
get_mac() ->
    {Stat, Ifs} = inet:getifaddrs(),
    case Stat of
        ok ->
            get_mac(Ifs);
        _ ->
            random_mac()
    end.

%% Generate a pseudo-random multicast MAC address.
-spec random_mac() -> integer().
random_mac() ->
    <<First:7, _:1, Rest:40>> = crypto:rand_bytes(6),
    <<Res:48>> = <<First:7, 1:1, Rest:40>>,
    Res.

%% Using MD5 and SHA1 with namespace UUIDs requires presenting some
%% UUID component in network (machine-independent) order. This code
%% enables interoperation with the reference code presented in the
%% standards. As a UUID is 128 bits, excess trailing bits are ignored.
-spec big_endian(binary() | integer()) -> binary().
big_endian(UUID) when is_binary(UUID), size(UUID) >= 16 ->
    <<TLo:32/native, TMid:16/native, VTHi:16/native, RSeq:16, Node:48,
      _/binary>> = UUID,
    <<TLo:32/big, TMid:16/big, VTHi:16/big, RSeq:16, Node:48>>;
big_endian(UUID) when is_integer(UUID) ->
    big_endian(<<UUID:128>>).

%% Present the specified number of bytes in hexadecimal.
-spec h(integer(), integer()) -> string().
h(1, Val) ->
    lists:flatten(io_lib:format("~2.16.0b", [Val]));
h(2, Val) ->
    lists:flatten(io_lib:format("~4.16.0b", [Val]));
h(4, Val) ->
    lists:flatten(io_lib:format("~8.16.0b", [Val]));
h(6, Val) ->
    lists:flatten(io_lib:format("~12.16.0b", [Val]));
h(7, Val) ->
    lists:flatten(io_lib:format("~14.16.0b", [Val]));
h(8, Val) ->
    lists:flatten(io_lib:format("~16.16.0b", [Val])).

%% Given the timestamp from a UUID, as defined by the standards,
%% decode it.
-spec dce_time(binary() | integer()) -> string() | error.
dce_time(<<Val:60>>) ->
    Tick70 = Val - ?OFFSET1582,                 % adjust to 1970
    Sec70 = Tick70 div 10000000,                % 100 nanoseconds per tick
    {M, S, _} = now(),
    case Sec70 =< M * 1000000 + S + 600 of
        true ->
            {Days, {Hr, Min, Sec}} = calendar:seconds_to_daystime(Sec70),
            {Yr, Mon, Day} = calendar:gregorian_days_to_date(Days),
            lists:flatten(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b",
                                        [Yr + 1970, Mon, Day, Hr, Min, Sec]));
        _ ->
            error
    end;
dce_time(Val) when is_integer(Val) ->
    dce_time(<<Val:60>>).

%% Given the timestamp from an NCS UUID, decode it.
-spec ncs_time(binary() | integer()) -> string() | error.
ncs_time(<<Val:48>>) ->
    Sec80 = Val div 250000,                     % 4 microseconds per tick
    Sec70 = Sec80 + ?OFFSET1980 * ?SECPERDAY,   % adjust from 1980 epoch
    {M, S, _} = now(),
    case Sec70 =< M * 1000000 + S of
        true ->
            {Days, {Hr, Min, Sec}} = calendar:seconds_to_daystime(Sec70),
            {Yr, Mon, Day} = calendar:gregorian_days_to_date(Days),
            lists:flatten(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b",
                                        [Yr + 1970, Mon, Day, Hr, Min, Sec]));
        _ ->
            error
    end;
ncs_time(Val) when is_integer(Val) ->
    ncs_time(<<Val:48>>).

%%% testing

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

mac_test() ->
    RanMAC = random_mac(),
    MAC = get_mac(),
    error_logger:info_msg("MAC ~12.16.0b Random ~12.16.0b~n", [MAC, RanMAC]),
    ok.

known_test() ->
    {ok, UUID} = space_uuid(dns),
    error_logger:info_msg("Known ~p~nKnown ~p~nKnown ~p~n",
                          [UUID, uuid:to_string(UUID), split_uuid(UUID)]),
    ok.

endian_test() ->
    {ok, UUID} = space_uuid(dns),
    error_logger:info_msg("Endian ~p native~nEndian ~p network~n",
                          [uuid:to_string(UUID), uuid:to_string(big_endian(UUID))]),
    ok.

ncs_test() ->
    State = init_state(),
    {reply, UUID, _} = ncs_uuid(State),
    Rep = uuid:to_string_ncs(UUID),
    % taken from a file within the Domain/OS SR10.4 release material
    UUID2 = <<16#34dc239ec000:48, 0:16, 16#0d00:16, 16#007c5f000000:48>>,
    Rep2 = uuid:to_string_ncs(UUID2),
    error_logger:info_msg("NCS ~p~nNCS ~p~nNCS ~p~nNCS ~p~nNCS ~p~nNCS ~p~n",
                          [uuid:to_string(UUID), Rep, split_uuid(UUID),
                           uuid:to_string(UUID2), Rep2, split_uuid(UUID2)]),
    ok.

time_test() ->
    State = init_state(),
    {reply, T1, State1} = time_uuid(State),
    {reply, T2, _} = time_uuid(State1),
    Diff = T1 /= T2,
    error_logger:info_msg("Time ~p~nTime ~p~nTime ~p~nTime ~p~nTime different ~p~n",
                          [uuid:to_string(T1), split_uuid(T1),
                           uuid:to_string(T2), split_uuid(T2), Diff]),
    Diff == true.

age_test() ->
    <<Given:128>> = <<16#252ab410:32, 16#7e7a:16, 16#11e4:16, 16#a868:16,
                      16#5ba5382ac590:48>>,
    {ok, Age} = age_uuid(Given),
    {Days, {Hr, Min, Sec}} = calendar:seconds_to_daystime(Age),
    AgeString = lists:flatten(io_lib:format("~bd ~2.10.0b:~2.10.0b:~2.10.0b",
                                            [Days, Hr, Min, Sec])),
    error_logger:info_msg(
      "Age ~p~nAge ~p~nAge ~p~nAge ~p~n",
      [uuid:to_string(Given), split_uuid(Given), Age, AgeString]),
    ok.

dce_test() ->
    State = init_state(),
    {reply, U1000, State1} = dce_sec_uuid(user, 1000, State),
    {reply, G20, _} = dce_sec_uuid(group, 20, State1),
    error_logger:info_msg("DCE ~p~nDCE ~p~nDCE ~p~nDCE ~p~n",
                          [uuid:to_string(U1000), split_uuid(U1000),
                           uuid:to_string(G20), split_uuid(G20)]),
    ok.

md5_test() ->
    Domain = "www.widgets.com",
    {ok, Have} = md5_uuid(Domain, dns),
    % erratum 1352 (RFC-4122)
    Expect = 16#3d813cbb47fb32ba91df831e1593ac29,
    {ok, Match} = match_uuid(Expect, Domain, dns),
    error_logger:info_msg(
      "MD5 Domain ~p~nMD5 ~p~nMD5 ~p~nMD5 ~p~nMD5 match ~p~n",
      [Domain, split_uuid(Have), uuid:to_string(Have),
       uuid:to_string(Expect), Match]),
    Match == true.

second_md5_test() ->
    Domain = "www.example.com",
    {ok, Have} = md5_uuid(Domain, dns),
    % erratum 3476 (RFC-4122)
    Expect = 16#5df418813aed351588a72f4a814cf09e,
    {ok, Match} = match_uuid(Expect, Domain, dns),
    error_logger:info_msg(
      "MD5 Domain ~p~nMD5 ~p~nMD5 ~p~nMD5 ~p~nMD5 match ~p~n",
      [Domain, split_uuid(Have), uuid:to_string(Have),
       uuid:to_string(Expect), Match]),
    Match == true.

sha1_test() ->
    Domain = "www.example.com",
    {ok, Have} = sha1_uuid(Domain),
    {ok, Have1} = sha1_uuid(Domain, dns),
    error_logger:info_msg(
      "SHA1 ~p~nSHA1 ~p~nSHA1 Domain ~p~nSHA1 ~p~nSHA1 ~p~n"
      "SHA1 No value to check against~n",
      [split_uuid(Have), uuid:to_string(Have),
       Domain, split_uuid(Have1), uuid:to_string(Have1)]),
    ok.

random_test() ->
    {ok, UUID} = random_uuid(),
    error_logger:info_msg("Random ~p~nRandom ~p~n",
                          [split_uuid(UUID), uuid:to_string(UUID)]),
    ok.

-endif.
