-module(aybabtu_lang).
-export([langs/0, tr/3, tr/4]).
 
-include("/usr/lib/yaws/include/yaws_api.hrl"). 
-include("../include/aybabtu_defines.hrl").

langs() ->
   [en, fr].

tr(Page, Section, A) ->
   tr(Page, Section, A, []).

tr(Page, Section, A, Params) ->
   Current_lang = case yaws_api:queryvar(A, "lang") of
      {ok, L} -> L;
      _ ->
         case yaws_api:find_cookie_val("lang", (A#arg.headers)#headers.cookie) of
         [] -> null;
         C -> C
      end
   end,
   io_lib:format(
      case Current_lang of
         "fr" ->
            translate(fr, Page, Section);
         _ ->
            translate(en, Page, Section)
      end,
      Params
   ).
   
translate(en, menu, home) -> "home";
translate(fr, menu, home) -> "home";

translate(en, menu, features) -> "features";
translate(fr, menu, features) -> "fonctionnalité";

translate(en, menu, faq) -> "FAQ";
translate(fr, menu, faq) -> "FAQ";

translate(en, menu, about) -> "about";
translate(fr, menu, about) -> "à propos";

%%%%%%%%%%

translate(en, home, title) -> "Aybabtu - The ultimate <acronym title=\"Local Area Network\">LAN</acronym> file sharing software.";
translate(fr, home, title) -> "Aybabtu - Le logiciel ultime de partage de fichiers en <acronym title=\"Local Area Network (Réseau local)\">LAN</acronym>.";

translate(en, home, description) -> "The goal is to easily exchange a massive amount of data in a local area network environment like a LAN-Party. After you launched Aybabtu, you will see all other people and theirs sharing automatically without special configuration. See the <a href=\"~s\">features list</a> for more information.";
translate(fr, home, description) -> "Le but est de permettre l'échange massif et facile de fichier sur un réseau local, par exemple lors d'une LAN-Party. Après avoir lancé Aybabtu, les autres personnes présentes sur le réseau sont visibles automatiquement sans aucune configuration particulière. Voir la <a href=\"~s\">page des fonctionnalités</a> pour plus d'informations.";

translate(en, home, warning_beta) -> "<em>Warning :</em> The actual version of Aybabtu is a beta, it's only for test purpose. You can report any defect <a href=\"~s\">here</a>. The latest beta may not be compatible with the previous ones.</p>";
translate(fr, home, warning_beta) -> "Attention : La version actuelle d'Aybabtu est une bêta, elle ne doit être utilisé qu'a des fins de tests. Il est possible de rapporter les anomalies rencontrées <a href=\"~s\">ici</a>. La compatibilité de la dernière bêta avec les précédentes n'est pas garanti.";

%%%%%%%%%%

translate(en, features, disclaimer) -> "<em>Here is the main features of the actual release.</em> Aybabtu is constantly under development, you can see <a href=\"~s\">here</a> the planned features.";
translate(fr, features, disclaimer) -> "<em>Voici la liste des principales fonctionnalités de la version courante.</em> Aybabtu est en développement constant, vous pouvez voir <a href=\"~s\">ici</a> les fonctionnalités planifiées pour les futures versions.";

translate(en, features, feat_1) -> "Share files and folders in a local area network environment (LAN). Subnets are supported.";
translate(fr, features, feat_1) -> "Partage de fichiers et dossiers sur un réseau local (LAN). Les sous-réseaux (<i>subnet</i>) sont supportés.";

translate(en, features, feat_2) -> "Distributed transfers to increase performance and reliability.";
translate(fr, features, feat_2) -> "Transferts distribués pour de meilleures performances et une meilleure fiabilité.";

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

translate(en, features, feat_8) -> "Aybabtu can run without graphic interface (GUI) and be controlled remotly."; 
translate(fr, features, feat_8) -> "Aybabtu peut être lancé sans interface graphique (<i>GUI</i>) et être piloté à distance.";

translate(en, features, feat_9) -> "Open source. Code source distributed under GPLv3 license.";
translate(fr, features, feat_9) -> "Open source. Le code est distribué sous la licence GPLv3.";

translate(en, features, help_us) -> "Don't forget to <a href=\"~s\">support us</a>. It will help to maintain and add new features.";
translate(fr, features, help_us) -> "N'oubliez pas de nous <a href=\"~s\">aider</a>, cela permettra la maintenance et l'ajout de nouvelles fonctionnalités";

%%%%%%%%%%

translate(en, faq, q1) -> "A new file sharing tool is neat but I can already share some files with my system tools. What are the benefits to use Aybabtu?";
translate(fr, faq, q1) -> "Un nouvel outil de partage de fichier c'est bien mais je peux déjà faire des partages avec les outils de mon système. Quels sont les avantages d'utiliser Aybabtu?";

translate(en, faq, a1) -> "Aybabtu is designed for massive transfers, you can manage a queue of files to be downloaded. A file may be downloaded automatically from many peers at the same time to speed up the transfer and prevent peer downtime. Aybabtu has a fast global search feature which default system file sharing doesn't have. You will find more information from the <a href=\"features.html\">feature page</a>.";
translate(fr, faq, a1) -> "Aybabtu est conçu pour des transfers massifs, il est possible de gérer une liste des fichiers à transferer. Un fichier peut être transferé depuis plusieurs pairs simultanément pour augmenter la vitesse ainsi que la fiabilité. Il est possible d'effectuer une recherche globale ce que les partages système par défaut n'ont pas. Pour plus d'informations voir la <a href=\"features.html\">page des fonctionnalités</a>.";

translate(en, faq, q2) -> "I don't see other computers in my network.";
translate(fr, faq, q2) -> "Je ne vois pas les autres ordinateurs de mon réseau.";

translate(en, faq, a2) ->
   "<ul>"
   "<li>Check you have the latest version of Aybabtu.</li>"
   "<li>Be sure the ports 59486 (<i>UDP</i>) and 59487 (<i>UDP + TCP</i>) are opened in your firewall.</li>"
   "<li>Be sure UDP multicast is allowed in your network.</li>"
   "</ul>";
translate(fr, faq, a2) ->
   "<ul>"
   "<li>Vérifier que la dernière version d'Aybabtu est installée.</li>"
   "<li>S'assurer que les ports 59486 (<i>UDP</i>) et 59487 (<i>UDP + TCP</i>) sont ouvert sur le pare-feu (<i>firewall</i>).</li>"
   "<li>S'assurer que l'UDP multicast est autorisé sur le réseau.</li>"
   "</ul>";
   
translate(en, faq, q3) -> "Where are put the downloaded files?";
translate(fr, faq, q3) -> "Où sont placés les fichiers transférés?";

translate(en, faq, a3) -> "In the first incoming folder which has enough free space. You can define these folders in the <i>Settings</i> tab. Usually one folder is common.";
translate(fr, faq, a3) -> "Dans le premier dossier de destination qui a assez d'espace libre. Les dossiers de destination sont définis dans l'onglet <i>Paramètres</i>. En général un seul dossier est suffisant.";

translate(en, faq, q4) -> "There is no Linux version!?";
translate(fr, faq, q4) -> "Il n'y a pas de version pour Linux!?";

translate(en, faq, a4) -> "A Linux version will come soon after the final 1.0 Windows release.";
translate(fr, faq, a4) -> "Une version Linux sera disponible après la sortie de la version 1.0 sous Windows.";

translate(en, faq, q5) -> "There is no Mac OS X version!?";
translate(fr, faq, q5) -> "Il n'y a pas de version pour Mac OS X!?";

translate(en, faq, a5) -> "Not yet, we are looking for a Mac OS X programmer.";
translate(fr, faq, a5) -> "Pas encore. Nous cherchons un développeur Mac OS X";

translate(en, faq, q6) -> "Can I configure Aybabtu to start automatically when my computer starting?";
translate(fr, faq, q6) -> "Est-il possible de configurer Aybabtu pour qu'il démarre automatiquement au démarrage de la machine?";

translate(en, faq, a6) -> "<i>Windows 7</i> : Go to <i>Control Panel</i> &gt; <i>Administrative Tools</i> &gt; <i>Services</i> . Open the properties of <i>Aybabtu Core</i> and set the <i>Statup type</i> from <i>Manual</i> to <i>Automatic</i>.";
translate(fr, faq, a6) -> "<i>Windows 7</i> : Allez dans <i>Panneau de configuration</i> > <i>Outils d'administrations</i> &gt; <i>Services</i>. Ouvrir la fenêtre de propriétés de <i>Aybabtu Core</i> et définir le <i>Type de Démarrage</i> à <i>Automatique</i>.";

%%%%%%%%%%

translate(en, about, author) -> "Author : ~s";
translate(fr, about, author) -> "Auteur : ~s";

translate(en, about, linux) -> "Linux maintainer : ~s";
translate(fr, about, linux) -> "Responsable Linux : ~s";

translate(en, about, thanks) -> "Thanks to ~s and ~s for their support.";
translate(fr, about, thanks) -> "Merci à ~s et ~s pour leur support.";

translate(en, about, tech) -> "Technologies and softwares used";
translate(fr, about, tech) -> "Technologies et logiciels utilisés";

translate(en, about, tech_used_aybabtu_title) -> "Aybabtu";
translate(fr, about, tech_used_aybabtu_title) -> "Aybabtu";

translate(en, about, tech_used_aybabtu) ->
   "<li>Programming language : <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Framework and libraries : <a href=\"http://qt.nokia.com/\">Qt 4</a></li>"
   "<li>Compiler : <a href=\"http://gcc.gnu.org/\">GCC</a> + <a href=\"http://www.mingw.org/\">MinGW</a></li>"
   "<li>Message serializer : <a href=\"http://code.google.com/p/protobuf/\">Protocol Buffers</a></li>"
   "<li>Random number generator : <a href=\"http://www-personal.umich.edu/~~wagnerr/MersenneTwister.html\">Mersenne Twister</a></li>"
   "<li>Cryptographic hash function : <a href=\"http://www.131002.net/blake/\">BLAKE</a></li>"
   "<li>Icons library : <a href=\"http://www.fatcow.com/free-icons\">FatCow</a></li>";
translate(fr, about, tech_used_aybabtu) ->
   "<li>Language de programmation : <a href=\"http://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
   "<li>Bibliothèque logicielle principale  : <a href=\"http://qt.nokia.com/\">Qt 4</a></li>"
   "<li>Compilateur : <a href=\"http://gcc.gnu.org/\">GCC</a> + <a href=\"http://www.mingw.org/\">MinGW</a></li>"
   "<li>Sérialisation des messages : <a href=\"http://code.google.com/p/protobuf/\">Protocol Buffers</a></li>"
   "<li>Générateur de nombres aléatoire : <a href=\"http://www-personal.umich.edu/~~wagnerr/MersenneTwister.html\">Mersenne Twister</a></li>"
   "<li>Fonction de hashage: <a href=\"http://www.131002.net/blake/\">BLAKE</a></li>"
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
   "<li>Constructeur d'installeur : <a href=\"http://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
   "<li>Éditeur vectoriel : <a href=\"http://inkscape.org/\">Inkscape</a></li>"
   "<li>Éditeur bitmap : <a href=\"http://www.gimp.org/\">GIMP</a></li>";
   
translate(en, about, tech_used_website_title) -> "Web site";
translate(fr, about, tech_used_website_title) -> "Site web";

translate(en, about, tech_used_website) ->
   "<li>Document structure : <a href=\"http://www.w3.org/TR/xhtml11/\">XHTML 1.1</a></li>"
   "<li>Document presentation : <a href=\"http://www.w3.org/TR/CSS21/\">CSS 2.1</a></li>"
   "<li>Client side dynamic language : <a href=\"http://fr.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>JavaScript libraries : <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Server side language : <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Web server : <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";
translate(fr, about, tech_used_website) ->
   "<li>Structure : <a href=\"http://www.w3.org/TR/xhtml11/\">XHTML 1.1</a></li>"
   "<li>Présentation : <a href=\"http://www.w3.org/TR/CSS21/\">CSS 2.1</a></li>"
   "<li>Langage dynamque coté client : <a href=\"http://fr.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
   "<li>Bibliothèques JavaScript : <a href=\"http://jquery.com/\">JQuery</a> + <a href=\"http://colorpowered.com/colorbox/\">ColorBox</a></li>"
   "<li>Langage coté serveur : <a href=\"http://www.erlang.org/\">Erlang</a></li>"
   "<li>Serveur web : <a href=\"http://yaws.hyber.org/\">Yaws</a></li>";

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

translate(en, download_button, download) -> "Download Aybabtu";
translate(fr, download_button, download) -> "Télécharger Aybabtu";

translate(en, download_button, version) -> "Version ~s for ~s";
translate(fr, download_button, version) -> "Version ~s pour ~s";

translate(en, download_button, released) -> "Released on ~s";
translate(fr, download_button, released) -> "Sorti le ~s";

translate(en, _, _) -> "No translation.";
translate(fr, _, _) -> "Pas de traduction";

translate(_, _, _) -> "".


