% This module will persist some information about the D-LAN website.
% Data are persisted as DETS table in the directory defined in "d_lan_defines.hrl". 
% The tables are described below, this first tuple element is the key.
%
% Downloads count:
%  Counts the number of download for each available files (releases + torrents).
%  The tuple structure is defined as:
%   {{<filename>, <date>}, n}. Where 'n' is the number of download and date a tuple {<year>, <month>, <day>}.
%
% Bitcoin addresses:
%  Save which bitcoin addresse corresponds to which IP. The goal is to avoid to generate more that one btc address for a given IP.
%  {<IP>, <bitcoin address>}.
%
% Donators:
%  Save all donators. Should be synchronized periodically with the bitcoin database
%  {{<name>, <date>}, <bitcoin address>, <link>, <amount>, <show_amount>}.

-module(d_lan_db).
-export([
   start/1,
   stop/0,
   new_download/1,
   nb_downloads_per_day/1,
   stats/1,
   all_files/0
]).

-include("/usr/lib/yaws/include/yaws.hrl"). 
-include("../include/d_lan_defines.hrl").

-define(DB_DOWNLOADS_COUNT, "d_lan_downloads_count.dets").
-define(DB_BITCOIN_ADDRESSES, "d_lan_bitcoin_addresses.dets").
-define(DB_DONATORS, "d_lan_donators.dets").

% Start the db service.
%
% Should be started as 'start_mod' by Yaws, see http://yaws.hyber.org/yman.yaws?page=yaws.conf.
start(Conf) ->
   % Prevent to start the service twice.
   case whereis(d_lan_db_process) of
      undefined ->
         % io:format("~p~n", [Conf]), % Debug.
         Filepath = Conf#sconf.docroot ++ "/" ++ ?DOWNLOAD_DB_FILEPATH ++ "/" ++ ?DB_DOWNLOADS_COUNT,
         io:format("Opening dets table :~p~n", [Filepath]),
         case dets:open_file(?DB_DOWNLOADS_COUNT, [{file, Filepath}]) of
            {ok, ?DB_DOWNLOADS_COUNT} -> 
               register(d_lan_db_process, self()),
               loop();
            {error, _Reason} ->
               io:format("Cannot open/create dets table :~p/~n", [Filepath])
         end;
      _ ->
         error_process_already_started
   end.

stop() ->
   d_lan_db_process ! stop,
   dets:close(?DB_DOWNLOADS_COUNT).

new_download(Filename) ->
   d_lan_db_process ! {new_download, Filename}.

nb_downloads_per_day(_Nb_months) ->
   0.
   %~ {Y, M, _} = now_utc(),
   %~ Now = {Y, M, 1},
   %~ map(
      %~ fun({Y, M, _}) -> 
         %~ format_result(dets:select(?DB_DOWNLOADS_COUNT, [{{{'$1', {Y, M,'$2'}}, '$3'}, [], [{{'$2', '$3'}}]}]), Y, M)
      %~ end,   
      %~ months_seq(Now, Nb_months - 1)
   %~ )   
   %~ foldl(fun(_, Acc) lists:seq(1, Nb_months)   
   
% [{d, n}] -> [{{y, m, d}, n}]
%~ format_result(Result, Year, Month) ->
   %~ foldl(
      %~ fun(Day, Acc) ->  seq(1, calendar:last_day_of_the_month(Year, Month)))

%~ format_result(Result, Year, Month, Day)
   
%~ months_seq(Month, 0) -> [];
%~ months_seq(Month, N) -> [Month | build_month_list(prev_date_by_one_month(Month), N - 1)].

%~ prev_date_by_one_month({Y, 1, _}) -> {Y - 1, 12, 1};
%~ prev_date_by_one_month({Y, M, _}) -> {Y, M - 1, 1}.

% Return a list of list: [[<date>, <number of d/l>], ...]. The list is sorted from the yougest to the oldest record.
% We don't use the process 'd_lan_db_process' because there is no concurrent access problem to just read a value.
stats(Filename) ->
   lists:reverse(lists:sort(
      fun([D1 | _], [D2 | _]) -> D1 < D2 end,
      case dets:match(?DB_DOWNLOADS_COUNT, {{Filename, '$1'}, '$2'}) of
         {error, _} -> [];
         R -> R
      end
   )).
   
% Returns a list of all the files.
all_files() ->
   lists:reverse(lists:sort(dets:foldl(fun({{Filename, _}, _}, Acc) -> case lists:member(Filename, Acc) of true -> Acc; _ -> [Filename | Acc] end end, [], ?DB_DOWNLOADS_COUNT))).

loop() ->
   receive
      {new_download, Filename} ->        
         Date = now_utc(),
         try
            case dets:lookup(?DB_DOWNLOADS_COUNT, {Filename, Date}) of
               [] ->
                  dets:insert(?DB_DOWNLOADS_COUNT, [{{Filename, Date}, 1}]);
               [{_, N}] ->
                  dets:insert(?DB_DOWNLOADS_COUNT, [{{Filename, Date}, N + 1}])
            end
         catch
            Exception ->
               io:format("An exception has occured when inserting a download count: ~p~n", [Exception])
         end,
         loop();
      stop ->
         ok
   end.
   
now_utc() ->
   {Date, _} = calendar:now_to_universal_time(now()),
   Date.
   
