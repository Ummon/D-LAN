-module(d_lan_download_counter).
-export([start/1, stop/0, new_download/1, stats/1, all_files/0]).

-include("/usr/lib/yaws/include/yaws.hrl"). 
-include("../include/d_lan_defines.hrl").

% Start the download counter service.
% Should be started as 'start_mod' by Yaws, see http://yaws.hyber.org/yman.yaws?page=yaws.conf.
start(Conf) ->
   % Prevent to start the service twice.
   case whereis(d_lan_download_counter_process) of
      undefined ->
         Filepath = Conf#sconf.docroot ++ "/" ++ ?DOWNLOAD_DB_FILEPATH,
         io:format("Opening dets table :~p~n", [Filepath]),
         case dets:open_file(?MODULE, [{file, Filepath}]) of
            {ok, ?MODULE} -> 
               %Pid = spawn(fun loop/0),
               register(d_lan_download_counter_process, self()),
               loop();
            {error, _Reason} ->
               io:format("Cannot open/create dets table :~p/~n", [Filepath])
         end;
      _ ->
         ok
   end.

stop() ->
   d_lan_download_counter_process ! stop,
   dets:close(?MODULE).

new_download(Filename) ->
   d_lan_download_counter_process ! {new_download, Filename}.

% Return a list of tuple: [[<date>, <number of d/l], ...]. The list is sorted from the yougest to the oldest record.
% We don't use the process 'd_lan_download_counter_proc' because there is no concurrent access problem to just read a value.
stats(Filename) ->
   lists:sort(
      fun([D1 | _], [D2 | _]) -> D1 < D2 end,
      case dets:match(?MODULE, {{Filename, '$1'}, '$2'}) of
         {error, _} -> [];
         R -> R
      end
   ).
   
% Returns a list of all the files.
all_files() ->
   dets:foldl(fun({{Filename, _}, _}, Acc) -> case lists:member(Filename, Acc) of true -> Acc; _ -> [Filename | Acc] end end, [], ?MODULE).

loop() ->
   receive
      {new_download, Filename} ->        
         Date = now_utc(),
         case dets:lookup(?MODULE, {Filename, Date}) of
            [] ->
               dets:insert(?MODULE, [{{Filename, Date}, 1}]),
               loop();
            [{_, N}] ->
               dets:insert(?MODULE, [{{Filename, Date}, N + 1}]),
               loop()
         end;
      stop ->
         ok
   end.
   
now_utc() ->
   {Date, _} = calendar:now_to_universal_time(now()),
   Date.
   