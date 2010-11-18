-module(aybabtu_common).
-export([current_page/1, page_name/1, menu/1, image/2, images/1, download_button/1, send_file/2]).
 
-include("/usr/lib/yaws/include/yaws_api.hrl"). 
-include("../include/aybabtu_defines.hrl").
 
pages() ->
   [home, features, about].
   
hidden_pages() -> [stats].
   
% Return the current page depending the page parameters -> atom()
current_page(A) ->
   case yaws_api:queryvar(A, "p") of
      {ok, PageStr} ->
         Page = list_to_atom(PageStr),
         case lists:member(Page, pages() ++ hidden_pages()) of
            true -> Page;
            _ -> unknown
         end;
      _ -> home
   end.
   
page_name(home) -> "home";
page_name(features) -> "features";
page_name(about) -> "about";
page_name(_) -> "unknown".

menu(A) ->
   Current_page = current_page(A),
   {ehtml,
      lists:map(
         fun(E) ->
            {li, [], [
               {a, [{href, atom_to_list(E) ++ ".html"}] ++ if Current_page =:= E -> [{class, "currentPage"}]; true -> [] end, page_name(E)}]}
         end,
         pages()
      )
   }.

image(Filename, Caption) ->
   {ehtml,
      [{'div', [{class, "gallery"}],
         [
            {a, [{href, "img/gallery/" ++ Filename ++ ".png"}, {rel, "group"}, {title, Caption}],
               [
                  {img, [{src, "img/gallery/" ++ Filename ++ "_thumb.png"}, {alt, Caption}]}
               ]
            },
            {p, [], Caption}
         ]
      }]
   }.
   
images(Filename_caption_list) ->
   {ehtml,
      lists:foldl(
         fun({Filename, Caption}, Acc) -> 
            {ehtml, [Div]} = image(Filename, Caption),
            [Acc | Div]
         end,
         [],
         Filename_caption_list
      )
   }.

download_button(A) ->
   {ok, Filenames} = file:list_dir(A#arg.docroot ++ "/" ++ ?RELEASES_FOLDER),
   Filename = lists:last(lists:sort(lists:filter(fun(F) -> string:right(F, 4) =:= ".exe" end, Filenames))),
   {match, [Version, Version_tag, Year, Month, Day]} = re:run(Filename, "Aybabtu-((?:\\d|\\.)+)([^-]*)-(\\d+)-(\\d+)-(\\d+).*", [{capture, all_but_first, list}]),
   File_size = filelib:file_size(A#arg.docroot ++ "/" ++ ?RELEASES_FOLDER ++ Filename),
   File_size_kb = trunc(File_size / 1024),
   {ehtml,
      [{'div', [{class, "download"}],
         [{a, [{href, atom_to_list(current_page(A)) ++ ".html&amp;dl=" ++ Filename}], 
            [
               {em, [], ["Download Aybabtu (" ++ integer_to_list(trunc(File_size_kb / 1024)) ++ "." ++ integer_to_list(trunc(((File_size_kb rem 1024) + 50) / 100)) ++ " MiB)"]}, {br},
               "Version " ++ Version ++ if Version_tag =/= [] -> " " ++ Version_tag; true -> [] end ++ " for Windows", {br},
               "Released on " ++ httpd_util:month(list_to_integer(Month)) ++ " " ++ Day ++ " " ++ Year ++ " UTC", {br}
               %"Number of download : " ++ integer_to_list(aybabtu_download_counter:nb_download(Filename))
            ]
         }]
      }]
   }.

% Send a file to the HTTP agent.
% See http://yaws.hyber.org/stream.yaws for more informations.
send_file(A, Filename) ->
   Filepath = A#arg.docroot ++ "/" ++ ?RELEASES_FOLDER ++ Filename,
   case file:open(Filepath, [read, binary]) of
      {ok, IoDevice} ->
         File_size = filelib:file_size(Filepath),
         send_stream(Filename, IoDevice),
         % See : http://en.wikipedia.org/wiki/List_of_HTTP_header_fields
         [{header, {content_length, File_size}}, {header, "Content-Disposition: attachment; filename=" ++ Filename}, {streamcontent, "application/octet-stream", <<>>}];
      _ -> ok
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
         aybabtu_download_counter:new_download(Filename);
      _ ->
         yaws_api:stream_chunk_end(YawsPid)
   end.
