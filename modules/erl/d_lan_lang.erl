-module(d_lan_lang).
-export([langs/0, plain_lang/1, current_lang/1, tr/3, tr/4]).
 
-include("/usr/lib/yaws/include/yaws_api.hrl"). 
-include("../include/d_lan_defines.hrl").

-spec langs() -> atom().
-spec plain_lang(atom()) -> string().
-spec current_lang(#arg{}) -> atom().
-spec tr(atom(), atom(), #arg{}) -> string().
-spec tr(atom(), atom(), #arg{}, [term()]) -> string().

% See here for the language codes : http://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
% Return a list of all accepted languages.
langs() ->
   [en, fr].
   
plain_lang(en) -> "English";
plain_lang(fr) -> "Français".
   
%%%%%%%%%%

translate(en, global, title) -> "D-LAN - A LAN file sharing software";
translate(fr, global, title) -> "D-LAN - Un logiciel de partage de fichiers en LAN";

%%%%%%%%%%

translate(en, menu, home) -> "HOME";
translate(fr, menu, home) -> "HOME";

translate(en, menu, features) -> "FEATURES";
translate(fr, menu, features) -> "FONCTIONNALITÉS";

translate(en, menu, faq) -> "FAQ";
translate(fr, menu, faq) -> "FAQ";

translate(en, menu, about) -> "ABOUT";
translate(fr, menu, about) -> "À PROPOS";

%%%%%%%%%%

translate(en, home, title) -> "D-LAN - A free <abbr title=\"Local Area Network\">LAN</abbr> file sharing software.";
translate(fr, home, title) -> "D-LAN - Un logiciel libre de partage de fichiers en <abbr title=\"Local Area Network (Réseau local)\">LAN</abbr>.";

translate(en, home, description) -> "The goal is to easily exchange a large amount of data in a local area network environment like a LAN-Party. After you launched D-LAN, you will see all other people and their sharing automatically without special configuration. See the <a href=\"~s\">features list</a> for more information.";
translate(fr, home, description) -> "Le but est de permettre l'échange massif et facile de fichiers sur un réseau local, par exemple lors d'une LAN-Party. Après avoir lancé D-LAN, les autres personnes présentes sur le réseau sont visibles automatiquement sans aucune configuration particulière. Voir la <a href=\"~s\">page des fonctionnalités</a> pour plus d'informations.";

translate(en, home, warning_beta) -> "<em>Warning:</em> The actual version of D-LAN is a beta, it's only for test purpose. You can report any defect <a href=\"~s\">here</a>. The latest beta may not be compatible with the previous ones.";
translate(fr, home, warning_beta) -> "<em>Attention :</em> La version actuelle de D-LAN est une bêta, elle ne doit être utilisée qu'à des fins de tests. Il est possible de rapporter les anomalies rencontrées <a href=\"~s\">ici</a>. La compatibilité de la dernière bêta avec les précédentes n'est pas garantie.";

%%%%%%%%%%

translate(en, features, disclaimer) -> "<em>Here is the main features of the actual release.</em> D-LAN is constantly under development, you can see <a href=\"~s\">here</a> the planned features.";
translate(fr, features, disclaimer) -> "<em>Voici la liste des principales fonctionnalités de la version courante.</em> D-LAN est en développement constant, vous pouvez voir <a href=\"~s\">ici</a> les fonctionnalités planifiées pour les futures versions.";

translate(en, features, feat_1) -> "Share files and folders in a local area network environment (LAN). Subnets are supported.";
translate(fr, features, feat_1) -> "Partage de fichiers et dossiers sur un réseau local (LAN). Les sous-réseaux (<i>subnet</i>) sont supportés.";

translate(en, features, feat_2) -> "Distributed transfers to increase performance and reliability.";
translate(fr, features, feat_2) -> "Transferts distribués pour de meilleures performances et une meilleure fiabilitée.";

translate(en, features, feat_3) -> "Very easy to use: no configuration, no central server.";
translate(fr, features, feat_3) -> "Très facile à utiliser, pas de configuration ni de serveur central.";

translate(en, features, feat_4) -> "Fast indexed search among all other peers.";
translate(fr, features, feat_4) -> "Recherche indexée rapide parmi l'ensemble des pairs.";

translate(en, features, feat_5) -> "Browse all files and folders of any other peer.";
translate(fr, features, feat_5) -> "Possibilité de naviguer dans les fichiers et dossiers des autres pairs";

translate(en, features, feat_6) -> "Manage the download queue. It includes adding, deleting or reordering.";
translate(fr, features, feat_6) -> "Gestion d'une liste des transferts. Il est possible d'ajouter, supprimer ou déplacer des transferts.";

translate(en, features, feat_7) -> "Global chatting.";
translate(fr, features, feat_7) -> "Chat global";

translate(en, features, feat_8) -> "D-LAN can run without graphic interface (GUI) and be controlled remotly."; 
translate(fr, features, feat_8) -> "D-LAN peut être lancé sans interface graphique (<i>GUI</i>) et être piloté à distance.";

translate(en, features, feat_9) -> "Open source. Code source distributed under GPLv3 license.";
translate(fr, features, feat_9) -> "Open source. Le code est distribué sous la licence GPLv3.";

translate(en, features, feat_10) -> "Free of any sort of ads or <a href=\"http://en.wikipedia.org/wiki/Malware\">malwares</a>.";
translate(fr, features, feat_10) -> "Ne contient aucune sorte de publicité ou de <a href=\"http://fr.wikipedia.org/wiki/Logiciel_malveillant\">'malware'</a>.";

translate(en, features, help_us) -> "Don't forget to <a href=\"~s\">support us</a>. It will help to maintain and add new features.";
translate(fr, features, help_us) -> "N'oubliez pas de nous <a href=\"~s\">aider</a>, cela permettra la maintenance et l'ajout de nouvelles fonctionnalités.";


%%%%%%%%%%

translate(en, faq, q1) -> "A new file sharing tool is neat but I can already share some files with my system tools. What are the benefits to use D-LAN?";
translate(fr, faq, q1) -> "Un nouvel outil de partage de fichier c'est bien mais je peux déjà faire des partages avec les outils de mon système. Quels sont les avantages d'utiliser D-LAN?";

translate(en, faq, a1) -> "D-LAN is designed for massive transfers, you can manage a queue of files to be downloaded. A file may be downloaded automatically from many peers at the same time to speed up the transfer and prevent peer downtime. D-LAN has a fast global search feature which default system file sharing doesn't have. You will find more information from the <a href=\"features.html\">feature page</a>.";
translate(fr, faq, a1) -> "D-LAN est conçu pour des transfers massifs, il est possible de gérer une liste des fichiers à transferer. Un fichier peut être transferé depuis plusieurs pairs simultanément pour augmenter la vitesse ainsi que la fiabilité. Il est possible d'effectuer une recherche globale, ce que les partages système par défaut n'ont pas. Pour plus d'informations voir la <a href=\"features.html\">page des fonctionnalités</a>.";

translate(en, faq, q2) -> "I don't see other computers in my network.";
translate(fr, faq, q2) -> "Je ne vois pas les autres ordinateurs de mon réseau.";

translate(en, faq, a2) ->
   "<ul>"
   "<li>Check you have the latest version of D-LAN.</li>"
   "<li>Be sure the ports 59486 (<i>UDP</i>) and 59487 (<i>UDP + TCP</i>) are opened in your firewall.</li>"
   "<li>Be sure UDP multicast is allowed in your network. The address used is this one: 236.123.43.24.</li>"
   "</ul>";
translate(fr, faq, a2) ->
   "<ul>"
   "<li>Vérifier que la dernière version de D-LAN est installée.</li>"
   "<li>S'assurer que les ports 59486 (<i>UDP</i>) et 59487 (<i>UDP + TCP</i>) sont ouvert sur le pare-feu (<i>firewall</i>).</li>"
   "<li>S'assurer que l'UDP multicast est autorisé sur le réseau. L'adresse utilisée est la suivante : 236.123.43.24.</li>"
   "</ul>";
   
translate(en, faq, q3) -> "There is no Linux version!?";
translate(fr, faq, q3) -> "Il n'y a pas de version pour Linux!?";

translate(en, faq, a3) -> "A Linux version will come soon after the final 1.0 Windows release.";
translate(fr, faq, a3) -> "Une version Linux sera disponible après la sortie de la version 1.0 sous Windows.";

translate(en, faq, q4) -> "There is no Mac OS X version!?";
translate(fr, faq, q4) -> "Il n'y a pas de version pour Mac OS X!?";

translate(en, faq, a4) -> "A Mac OS X version is planned for summer 2011.";
translate(fr, faq, a4) -> "Une version Mac OS X est prévue pour l'été 2011.";

translate(en, faq, q5) -> "Can I configure D-LAN to start automatically when my computer starting?";
translate(fr, faq, q5) -> "Est-il possible de configurer D-LAN pour qu'il démarre automatiquement au démarrage de la machine?";

translate(en, faq, a5) -> "<i>Windows 7</i> : Go to <i>Control Panel</i> &gt; <i>Administrative Tools</i> &gt; <i>Services</i> . Open the properties of <i>D-LAN Core</i> and set the <i>Statup type</i> from <i>Manual</i> to <i>Automatic</i>.";
translate(fr, faq, a5) -> "<i>Windows 7</i> : Allez dans <i>Panneau de configuration</i> > <i>Outils d'administrations</i> &gt; <i>Services</i>. Ouvrir la fenêtre de propriétés de <i>D-LAN Core</i> et définir le <i>Type de Démarrage</i> à <i>Automatique</i>.";

%%%%%%%%%%

translate(en, about, author) -> "Author : ~s";
translate(fr, about, author) -> "Auteur : ~s";

translate(en, about, linux) -> "Linux maintainer : ~s";
translate(fr, about, linux) -> "Responsable Linux : ~s";

translate(en, about, thanks) -> "Thanks to ~s and ~s for their support.";
translate(fr, about, thanks) -> "Merci à ~s et ~s pour leur support.";

translate(en, about, tech) -> "Technologies and softwares used";
translate(fr, about, tech) -> "Technologies et logiciels utilisés";

translate(en, about, tech_used_d_lan_title) -> "D-LAN";
translate(fr, about, tech_used_d_lan_title) -> "D-LAN";

translate(en, about, tech_used_d_lan) ->
   "<li>Programming language : <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Framework and libraries : <a href=\"http://qt.nokia.com/\">Qt 4</a></li>"
   "<li>Compiler : <a href=\"http://gcc.gnu.org/\">GCC</a> + <a href=\"http://www.mingw.org/\">MinGW</a></li>"
   "<li>Message serializer : <a href=\"http://code.google.com/p/protobuf/\">Protocol Buffers</a></li>"
   "<li>Random number generator : <a href=\"http://www-personal.umich.edu/~~wagnerr/MersenneTwister.html\">Mersenne Twister</a></li>"
   "<li>Cryptographic hash function : <a href=\"http://en.wikipedia.org/wiki/SHA-1\">SHA-1</a></li>"
   "<li>Icons library : <a href=\"http://www.fatcow.com/free-icons\">FatCow</a></li>";
translate(fr, about, tech_used_d_lan) ->
   "<li>Language de programmation : <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Bibliothèque logicielle principale  : <a href=\"http://qt.nokia.com/\">Qt 4</a></li>"
   "<li>Compilateur : <a href=\"http://gcc.gnu.org/\">GCC</a> + <a href=\"http://www.mingw.org/\">MinGW</a></li>"
   "<li>Sérialisation des messages : <a href=\"http://code.google.com/p/protobuf/\">Protocol Buffers</a></li>"
   "<li>Générateur de nombres aléatoire : <a href=\"http://www-personal.umich.edu/~~wagnerr/MersenneTwister.html\">Mersenne Twister</a></li>"
   "<li>Fonction de hashage: <a href=\"http://fr.wikipedia.org/wiki/SHA-1\">SHA-1</a></li>"
   "<li>Bibliothèque d'icones : <a href=\"http://www.fatcow.com/free-icons\">FatCow</a></li>";
   
   
translate(en, about, tech_used_tools_title) -> "Developpment tools";
translate(fr, about, tech_used_tools_title) -> "Outils de développement";

translate(en, about, tech_used_tools) ->
   "<li>Development environment : <a href=\"http://qt.nokia.com/products/developer-tools/\">Qt Creator</a></li>"
   "<li>Version control system : <a href=\"http://git-scm.com/\">git</a> + <a href=\"http://code.google.com/p/msysgit/\">msysgit</a> + <a href=\"http://code.google.com/p/tortoisegit/\">Tortoise Git</a></li>"
   "<li>Scripting : <a href=\"http://www.gnu.org/software/bash/bash.html\">Bash</a></li>"
   "<li>Lightweight editor : <a href=\"http://www.scintilla.org/SciTE.html\">SciTE</a></li>"
   "<li>Project management : <a href=\"http://www.redmine.org/\">Redmine</a></li>"
   "<li>Documentation generator : <a href=\"http://www.doxygen.org/\">Doxygen</a></li>"
   "<li>UML editor : <a href=\"http://bouml.free.fr/\">Bouml</a></li>"
   "<li>Setup builder : <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>Vector graphics editor : <a href=\"http://inkscape.org/\">Inkscape</a></li>"
   "<li>Bitmap graphics editor : <a href=\"http://www.gimp.org/\">GIMP</a></li>";
translate(fr, about, tech_used_tools) ->
   "<li>Environnement de développement : <a href=\"http://qt.nokia.com/products/developer-tools/\">Qt Creator</a></li>"
   "<li>Système de gestion de versions : <a href=\"http://git-scm.com/\">git</a> + <a href=\"http://code.google.com/p/msysgit/\">msysgit</a> + <a href=\"http://code.google.com/p/tortoisegit/\">Tortoise Git</a></li>"
   "<li>Scripting : <a href=\"http://www.gnu.org/software/bash/bash.html\">Bash</a></li>"
   "<li>Éditeur léger : <a href=\"http://www.scintilla.org/SciTE.html\">SciTE</a></li>"
   "<li>Gestion de projet : <a href=\"http://www.redmine.org/\">Redmine</a></li>"
   "<li>Génération de la documentation : <a href=\"http://www.doxygen.org/\">Doxygen</a></li>"
   "<li>Éditeur UML : <a href=\"http://bouml.free.fr/\">Bouml</a></li>"
   "<li>Système d'installation : <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>Éditeur vectoriel : <a href=\"http://inkscape.org/\">Inkscape</a></li>"
   "<li>Éditeur bitmap : <a href=\"http://www.gimp.org/\">GIMP</a></li>";
   
translate(en, about, tech_used_website_title) -> "Web site";
translate(fr, about, tech_used_website_title) -> "Site web";

translate(en, about, tech_used_website) ->
   "<li>Document structure : <a href=\"http://www.w3.org/TR/html5/\">HTML5</a></li>"
   "<li>Document presentation : <a href=\"http://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"http://sass-lang.com\">Sass</a></li>"
   "<li>Client side dynamic language : <a href=\"http://fr.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>JavaScript libraries : <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Server side language : <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Web server : <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";
translate(fr, about, tech_used_website) ->
   "<li>Structure : <a href=\"http://dev.w3.org/html5/spec/Overview.html\">HTML 5</a></li>"
   "<li>Présentation : <a href=\"http://www.w3.org/TR/css3-roadmap/\">CSS3</a> + <a href=\"http://sass-lang.com\">Sass</a></li>"
   "<li>Langage dynamque coté client : <a href=\"http://fr.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>Bibliothèques JavaScript : <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Langage coté serveur : <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Serveur web : <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";

%%%%%%%%%%

translate(en, donate, title) -> "Support us";
translate(fr, donate, title) -> "Soutenez-nous";

translate(en, donate, intro) -> "If you like this project and want to see it grow, support us!";
translate(fr, donate, intro) -> "Si vous aimez ce projet et que vous voulez le voir grandir, soutenez-nous !";

translate(en, donate, bitcoin_address) -> "Bitcoin address: ";
translate(fr, donate, bitcoin_address) -> "Adresse bitcoin : ";

translate(en, donate, flattr_info) -> "Flattr is a social micro-payment system.";
translate(fr, donate, flattr_info) -> "Flattr est un système de micro-paiements.";

%%%%%%%%%%

translate(en, gallery, browsing) -> "Browsing";
translate(fr, gallery, browsing) -> "Navigation";

translate(en, gallery, download) -> "Download queue";
translate(fr, gallery, download) -> "Liste des transferts";

translate(en, gallery, search_result) -> "Search result";
translate(fr, gallery, search_result) -> "Résultat de la recherche";

translate(en, gallery, search_feature) -> "Search feature";
translate(fr, gallery, search_feature) -> "Recherche";

%%%%%%%%%%

translate(en, download_button, download) -> "Download D-LAN";
translate(fr, download_button, download) -> "Télécharger D-LAN";

translate(en, download_button, version) -> "Version ~s for ~s";
translate(fr, download_button, version) -> "Version ~s pour ~s";

translate(en, download_button, released) -> "Released on ~s";
translate(fr, download_button, released) -> "Sorti le ~s";

translate(en, _, _) -> "No translation.";
translate(fr, _, _) -> "Pas de traduction";

translate(_, _, _) -> "".

%%%%%%%%%%

% Return the current language depending the 'lang' GET ou the cookies.
% Contained in 'langs()'.
current_lang(A) ->
   % 1) Looks if a GET variable 'lang' is defined
   case yaws_api:queryvar(A, "lang") of
      {ok, L} -> list_to_lang(L);
      _ ->
         % 2) Looks if a 'lang' value exist in a cookie.
         case yaws_api:find_cookie_val("lang", (A#arg.headers)#headers.cookie) of
         [] ->
            % 3) Looks in the "Accept-Language" HTTP header field.
            case accepted_langs_by_user_agent(A) of
               [Lang | _] -> Lang;
               _ -> hd(langs())
            end;
         C -> list_to_lang(C)
      end
   end.
   
% Returns a known atom language from a string().
% Returns the first known language if the given string is unknown.
% See 'langs()'.
list_to_lang(Lang_str) ->
   try list_to_existing_atom(Lang_str) of
      Lang ->
         case lists:member(Lang, langs()) of
            true -> Lang;
            _ -> hd(langs()) % The language isn't defined (or known) we take the first of the list.
         end
   catch
      error:_ -> hd(langs())
   end.

% Return a list of accepted languages by the user agent. Return only known languages from 'langs/0'.
% Read the HTTP field 'Accept-Language'.
accepted_langs_by_user_agent(A) ->
   lists:map(
      fun ({Lang, _}) -> Lang end, % Remove the quality information.
      lists:reverse(lists:keysort(2, % Sort by quality, bigger first.
         case lists:filter(fun({http_header, _, Name, _, _}) -> Name =:= 'Accept-Language' end, (A#arg.headers)#headers.other) of
            [{http_header, _, _, _, Values} | _] ->
               % erlang:display(Values), % For debug purpose.
               lists:foldr(
                  fun(Val, Acc) ->
                     {Lang_str_with_subtag, Quality} = case string:tokens(string:strip(Val), ";") of
                        [L, "q=" ++ Q] -> {L, list_to_float(Q)};
                        [L | _] -> {L, 1.0}
                     end,
                     [Lang_str | _] = string:tokens(Lang_str_with_subtag, "-"), % We don't care about the subtags.
                     try list_to_existing_atom(Lang_str) of
                        Lang -> 
                           case lists:member(Lang, langs()) of
                              true -> [{Lang, Quality} | Acc]; % We keep only known languages.
                              _ -> Acc
                           end 
                     catch
                        error:_ -> Acc
                     end
                  end,
                  [],
                  string:tokens(Values, ",")
               );
            _ -> []
         end
      ))
   ).

tr(Page, Section, A) ->
   tr(Page, Section, A, []).

tr(Page, Section, A, Params) ->
   io_lib:format(translate(current_lang(A), Page, Section), Params).

