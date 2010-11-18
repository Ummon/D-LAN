-module(aybabtu_download_counter).
-export([start/1, stop/0, new_download/1, nb_download/1, all_files/0, reset/1]).

-include("/usr/lib/yaws/include/yaws.hrl"). 
-include("../include/aybabtu_defines.hrl").

% Start the download counter service.
% Should be started as 'start_mod' by Yaws, see http://yaws.hyber.org/yman.yaws?page=yaws.conf.
start(Conf) ->
   % Prevent to start the service twice.
   case whereis(aybabtu_download_counter_process) of
      undefined ->
         Filepath = Conf#sconf.docroot ++ "/" ++ ?DOWNLOAD_DB_FILEPATH,
         io:format("Opening dets table :~p~n", [Filepath]),
         case dets:open_file(?MODULE, [{file, Filepath}]) of
            {ok, ?MODULE} -> 
               %Pid = spawn(fun loop/0),
               register(aybabtu_download_counter_process, self()),
               loop();
            {error, _Reason} ->
               io:format("Cannot open/create dets table :~p/~n", [Filepath])
         end;
      _ ->
         ok
   end.

stop() ->
   aybabtu_download_counter_process ! stop,
   dets:close(?MODULE).

new_download(Filename) ->
   aybabtu_download_counter_process ! {new_download, Filename}.

% We don't use the process 'aybabtu_download_counter_proc' because there is no concurrent access problem to just read a value.
nb_download(Filename) ->
   case dets:lookup(?MODULE, Filename) of
      [] -> 0;
      [{_, N}] -> N
   end.
   
all_files() ->
   dets:foldl(fun(File, Acc) -> [File | Acc] end, [], ?MODULE).

reset(Filename) ->
   aybabtu_download_counter_process ! {reset, Filename}.

loop() ->
   receive
      {new_download, Filename} ->
         case dets:lookup(?MODULE, Filename) of
            [] ->
               dets:insert(?MODULE, [{Filename, 1}]),
               loop();
            [{_, N}] ->
               dets:insert(?MODULE, [{Filename, N + 1}]),
               loop()
         end;
      {reset, Filename} ->
         dets:insert(?MODULE, [{Filename, 0}]),
         loop();
      stop ->
         ok
   end.