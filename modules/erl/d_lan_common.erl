-module(d_lan_common).
-export([get_data/1, current_page/1, page_name/2, menu/1, image/2, image/3, images/1, download_button/2, send_release/3]).

-import(d_lan_lang, [tr/3, tr/4]).
-import(lists, [member/2, map/2, last/1, sort/1]).
 
-include("/usr/lib/yaws/include/yaws_api.hrl"). 
-include("../include/d_lan_defines.hrl").

-define(MAIN_PAGE, "yssi/main.yaws").

-type page() :: atom().
-spec current_page(#arg{}) -> page() | unknown.
-spec page_name(#arg{}, page()) -> string().
-spec menu(#arg{}) -> {ehtml, term()}.
-spec image(string(), string()) -> {'div', term()}.
-spec download_button(#arg{}, string()) -> {'div', term()}.
-spec send_release(#arg{}, string(), string()) -> [#headers{}] | error.

pages() ->
   [home, features, faq, about].

hidden_pages() ->
   [stats, donate].
   
get_data(A) ->
   % We ignore the page.
   case yaws_api:parse_query(A) of
      [_p, {"get_stat_data", Nb_months}] ->
         {content, "text/plain", lists:foldl(fun(N, Acc) -> Acc ++ integer_to_list(N) ++ "~n" end, "", d_lan_db:nb_downloads_per_day(Nb_months))};
      [_p, {"dl", Filename}, {"platform", Platform}] ->
         d_lan_common:send_release(A, Filename, Platform);
      [_p, {"lang", Lang}] ->
         [
            yaws_api:setcookie("lang", Lang, "/"),
            {yssi, ?MAIN_PAGE}
         ];
      _ ->      
         {yssi, ?MAIN_PAGE}     
   end.
   
% Return the current page depending the page parameters.
current_page(A) ->
   case yaws_api:queryvar(A, "p") of
      {ok, PageStr} ->
         try list_to_existing_atom(PageStr) of
            Page -> 
               case member(Page, pages() ++ hidden_pages()) of
                  true -> Page;
                  _ -> unknown
               end
         catch
            error:_ -> unknown
         end;
      _ -> home
   end.
   
page_name(A, P) -> tr(menu, P, A).

menu(A) ->
   Current_page = current_page(A),
   {ehtml,
      map(
         fun(E) ->
            {li, [], [
               {a, [{href, atom_to_list(E) ++ ".html"}] ++ if Current_page =:= E -> [{class, "current-page"}]; true -> [] end, page_name(A, E)}]}
         end,
         pages()
      )
   }.

image(Filename, Caption) ->
   image(Filename, Caption, Caption).
image(Filename, Caption, Comment) ->
   {'div', [{class, "box gallery"}],
      [
         {a, [{href, "img/gallery/" ++ Filename ++ ".png"}, {rel, "group"}, {title, Comment}],
               "<img src = \"img/gallery/" ++ Filename ++ "_thumb.png\" alt=\"" ++ Caption ++ "\" />"
         },
         {p, [], Caption}
      ]
   }.
   
images(Filename_caption_list) ->
   map(
      fun
         ({Filename, Caption}) -> 
            image(Filename, Caption);
         ({Filename, Caption, Comment}) ->
            image(Filename, Caption, Comment)
      end,
      Filename_caption_list
   ).
   
% 'Platform' is a folder where the releases are put.
% For example: "windows".
download_button(A, Platform) ->
   Platform_formatted = [string:to_upper(hd(Platform)) | tl(Platform)],
   Relase_platform_folder = A#arg.docroot ++ "/" ++ ?RELEASES_FOLDER ++ "/" ++ Platform,
   {ok, Filenames} = file:list_dir(Relase_platform_folder),
   Filename = last(sort([F || F <- Filenames, lists:member(string:right(F, 4), [".exe", ".dmg", ".deb"])])),
   Extension = string:right(Filename, 3),
   % 'Archi' isn't used for the moment.
   {match, [Version, Version_tag, Year, Month, Day, Archi]} =
      case Extension of
         "deb" ->
            re:run(Filename, "D-LAN-((?:\\d|\\.)+)([^-]*)-(\\d+)-(\\d+)-(\\d+)_.*-(\\w+)\\..*", [{capture, all_but_first, list}]);      
         _ ->
            {match, Values} = re:run(Filename, "D-LAN-((?:\\d|\\.)+)([^-]*)-(\\d+)-(\\d+)-(\\d+).*\\..*", [{capture, all_but_first, list}]),
            {match, Values ++ [""]} % Archi is empty on Windows.
      end,
   File_size = filelib:file_size(Relase_platform_folder ++ "/" ++ Filename),
   File_size_mb = float(File_size) / 1048576,
   {'div', [{class, "download" ++ " " ++ Extension}],
      [{a, [{class, "installer"}, {href, file_to_url(A, Filename, Platform)}], 
         [
            {em, [], [tr(download_button, download, A) ++ " (" ++ io_lib:format("~.2f", [File_size_mb]) ++ " MiB)"]}, {br},
            tr(download_button, version, A,
               [Version ++ if Version_tag =/= [] -> " " ++ Version_tag; true -> [] end, Platform_formatted]
            ), {br},
            tr(download_button, released, A, [httpd_util:month(list_to_integer(Month)) ++ " " ++ Day ++ " " ++ Year])
            %"Number of download : " ++ integer_to_list(d_lan_download_db(Filename))
         ]
      }] 
      ++ % Add a link to the torrent file if it exists.
      case lists:reverse(sort([F || F <- Filenames, string:right(F, 8) =:= ".torrent"])) of
         [Torrent_file | _] ->
            [{a, [{class, "torrent"}, {href, file_to_url(A, Torrent_file, Platform)}],
               tr(download_button, torrent, A)
            }];
         _ -> []
      end
   }.

% Returns the url to download a given file for the given platform.
file_to_url(A, Filename, Platform) ->
   atom_to_list(current_page(A)) ++ ".html&amp;dl=" ++ Filename ++ "&amp;platform=" ++ Platform.

% Send a file to the HTTP agent.
% See http://yaws.hyber.org/stream.yaws for more informations.
send_release(A, Filename, Platform) ->
   Filepath = A#arg.docroot ++ "/" ++ ?RELEASES_FOLDER  ++ "/" ++ Platform ++ "/" ++ Filename,
   case file:open(Filepath, [read, binary]) of
      {ok, IoDevice} ->
         File_size = filelib:file_size(Filepath),
         send_stream(Filename, IoDevice),
         % See : http://en.wikipedia.org/wiki/List_of_HTTP_header_fields
         [{header, {content_length, File_size}}, {header, "Content-Disposition: attachment; filename=" ++ Filename}, {streamcontent, "application/octet-stream", <<>>}];
      _ -> error
   end.

send_stream(Filename, IoDevice) ->
   YawsPid = self(),
   spawn(
      fun() -> send_stream_loop(Filename, IoDevice, YawsPid) end
   ).
send_stream_loop(Filename, IoDevice, YawsPid) ->
   case file:read(IoDevice, 4 * 1024) of
      {ok, Data} ->
         yaws_api:stream_chunk_deliver(YawsPid, Data),
         send_stream_loop(Filename, IoDevice, YawsPid);
      eof ->
         yaws_api:stream_chunk_end(YawsPid),
         % Increment the number of download
         d_lan_db:new_download(Filename);
      _ ->
         yaws_api:stream_chunk_end(YawsPid)
   end.
