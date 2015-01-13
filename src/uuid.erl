%% -*- mode: erlang; indent-tabs-mode: nil -*-
% Copyright (c) 2012-2015 Christopher Vance.
% Copyright (c) 2012-2013 Into Science Pty Ltd.
-module(uuid).
-author('Christopher Vance').

-export([time/0,dce_security/2,md5_hash/2,md5_hash/1,random/0,
         sha1_hash/2,sha1_hash/1,ncs/0]).
-export([split/1,match/3,to_string/1,to_string_ncs/1,from_string/1,age/1]).

%%% uuid client api

%% Create a time-based UUID.
-spec time() -> integer().
time() ->
    gen_server:call(uuid_server, {time}).

%% Create a DCE security UUID for the specified user or group.
-spec dce_security(user | group, integer()) -> integer().
dce_security(user, User) when is_integer(User) ->
    gen_server:call(uuid_server, {dce_security, user, User});
dce_security(group, Group) when is_integer(Group) ->
    gen_server:call(uuid_server, {dce_security, group, Group}).

%% Create an MD5 or SHA1 namespace UUID within a namespace.
-spec md5_hash(string(), dns | url | oid | x500dn) -> integer().
md5_hash(Name, Space) when is_atom(Space) ->
    gen_server:call(uuid_server, {md5_hash, Name, Space}).
-spec sha1_hash(string(), dns | url | oid | x500dn) -> integer().
sha1_hash(Name, Space) when is_atom(Space) ->
    gen_server:call(uuid_server, {sha1_hash, Name, Space}).

%% Create an MD5 or SHA1 namespace UUID without a namespace.
-spec md5_hash(string()) -> integer().
md5_hash(Name) ->
    gen_server:call(uuid_server, {md5_hash, Name}).
-spec sha1_hash(string()) -> integer().
sha1_hash(Name) ->
    gen_server:call(uuid_server, {sha1_hash, Name}).

%% Create a pseudo-random UUID.
-spec random() -> integer().
random() ->
    gen_server:call(uuid_server, {random}).

%% Create an NCS format UUID (precedes the standards used above). Obsolete.
-spec ncs() -> integer().
ncs() ->
    gen_server:call(uuid_server, {ncs}).

%% Split a UUID into components.
-spec split(integer()) -> list().
split(UUID) when is_integer(UUID) ->
    gen_server:call(uuid_server, {split, UUID}).

%% Is the UUID a namespace UUID for the name and namespace.
-spec match(integer(), string(), dns | url | oid | x500dn) -> true | false.
match(UUID, Name, Space) when is_integer(UUID) ->
    gen_server:call(uuid_server, {match, UUID, Name, Space}).

%% Convert a UUID into the usual string format.
-spec to_string(binary() | integer()) -> string().
to_string(<<UUID:128>>) ->
    <<TLo:32, TMid:16, VerTHi:16, ResSeq:16, Node:48>> = <<UUID:128>>,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                [TLo, TMid, VerTHi, ResSeq, Node]));
to_string(UUID) when is_integer(UUID) ->
    to_string(<<UUID:128>>).

%% Convert an NCS UUID into the old string format.
-spec to_string_ncs(binary() | integer()) -> string() | {error, term()}.
to_string_ncs(<<UUID:128>>) ->
    <<TLo:32, TMid:16, VerTHi:16, ResSeq:16, Node:48>> = <<UUID:128>>,
    <<Res:1, _:15>> = <<ResSeq:16>>,
    <<RSHi:8, SLo:8>> = <<ResSeq:16>>,
    <<N0:8, N1:8, N2:8, N3:8, N4:8, N5:8>> = <<Node:48>>,
    Rep = lists:flatten(io_lib:format("~8.16.0b~4.16.0b.~2.16.0b.~2.16.0b.~2.16.0b.~2.16.0b.~2.16.0b.~2.16.0b.~2.16.0b.~2.16.0b",
                                      [TLo, TMid, RSHi, SLo, N0, N1, N2, N3, N4, N5])),
    case Res == 0 andalso VerTHi == 0 of
        true ->
            Rep;
        _ ->
            {error, format}
    end;
to_string_ncs(UUID) when is_integer(UUID) ->
    to_string_ncs(<<UUID:128>>).

%% Convert a string format UUID to internal (integer) form.
-spec from_string(string()) -> integer().
from_string([${, A1, A2, A3, A4, A5, A6, A7, A8,
             $-, B1, B2, B3, B4, $-, C1, C2, C3, C4,
             $-, D1, D2, D3, D4, $-, E1, E2, E3, E4,
             E5, E6, E7, E8, E9, E10, E11, E12, $}]) ->
    erlang:list_to_integer([A1, A2, A3, A4, A5, A6, A7, A8,
                            B1, B2, B3, B4, C1, C2, C3, C4,
                            D1, D2, D3, D4, E1, E2, E3, E4,
                            E5, E6, E7, E8, E9, E10, E11, E12], 16);
from_string([A1, A2, A3, A4, A5, A6, A7, A8,
             $-, B1, B2, B3, B4, $-, C1, C2, C3, C4,
             $-, D1, D2, D3, D4, $-, E1, E2, E3, E4,
             E5, E6, E7, E8, E9, E10, E11, E12]) ->
    erlang:list_to_integer([A1, A2, A3, A4, A5, A6, A7, A8,
                            B1, B2, B3, B4, C1, C2, C3, C4,
                            D1, D2, D3, D4, E1, E2, E3, E4,
                            E5, E6, E7, E8, E9, E10, E11, E12], 16);
from_string([A1, A2, A3, A4, A5, A6, A7, A8,
             B1, B2, B3, B4, C1, C2, C3, C4,
             D1, D2, D3, D4, E1, E2, E3, E4,
             E5, E6, E7, E8, E9, E10, E11, E12]) ->
    erlang:list_to_integer([A1, A2, A3, A4, A5, A6, A7, A8,
                            B1, B2, B3, B4, C1, C2, C3, C4,
                            D1, D2, D3, D4, E1, E2, E3, E4,
                            E5, E6, E7, E8, E9, E10, E11, E12], 16);
from_string([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12,
             $., B1, B2, $., C1, C2, $., D1, D2, $., E1, E2,
             $., F1, F2, $., G1, G2, $., H1, H2, $., I1, I2]) ->
    A = erlang:list_to_integer([A1, A2, A3, A4, A5, A6, A7, A8,
                                A9, A10, A11, A12], 16),
    B = erlang:list_to_integer([B1, B2, C1, C2, D1, D2, E1, E2,
                                F1, F2, G1, G2, H1, H2, I1, I2], 16),
    <<Val:128>> = <<A:48, 0:16, B:64>>,
    Val.

%% Determine the age (in seconds) of a time-based UUID.
-spec age(integer()) -> integer() | undefined.
age(UUID) when is_integer(UUID) ->
    gen_server:call(uuid_server, {age, UUID}).

%%% functions internal to uuid implementation

%%% testing

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_write(Input) ->
    Read = from_string(Input),
    Output = to_string(Read),
    error_logger:info_msg("Input  ~p~nRead   ~32.16.0b~nResult ~p~n",
                          [Input, Read, Output]).

format_test() ->
    read_write("{01234567-89AB-4CDE-8F01-001234ABCDEF}"),
    read_write("89ABCDEF-0123-4567-89AB-00CDEF012345"),
    read_write("456789ABCDEF412345670089ABCDEF01"),
    A = "333344445555.0D.00.01.02.03.04.05.06",
    read_write(A),
    error_logger:info_msg("Old    ~p~n", [to_string_ncs(from_string(A))]).

-endif.
