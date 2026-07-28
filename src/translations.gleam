import gleam/float
import gleam/http/request
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lustre/element
import lustre/element/html
import wisp

pub type Lang {
  En
  Fr
  De
  Es
  It
  Ru
  Ko
  Ja
}

pub fn all_langs() -> List(Lang) {
  [En, Fr, De, Es, It, Ru, Ko, Ja]
}

pub fn plain_lang(l: Lang) {
  case l {
    En -> "English"
    Fr -> "Français"
    De -> "Deutsch"
    Es -> "Español"
    It -> "Italiano"
    Ru -> "Русский"
    Ko -> "한국어"
    Ja -> "日本語"
  }
}

fn parse_lang(lang_str: String) -> Lang {
  case lang_str {
    "en" -> En
    "fr" -> Fr
    "de" -> De
    "es" -> Es
    "it" -> It
    "ru" -> Ru
    "ko" -> Ko
    "ja" -> Ja
    _ -> En
  }
}

pub fn to_str(l: Lang) -> String {
  case l {
    En -> "en"
    Fr -> "fr"
    De -> "de"
    Es -> "es"
    It -> "it"
    Ru -> "ru"
    Ko -> "ko"
    Ja -> "ja"
  }
}

fn raw_span(html_string: String) -> element.Element(a) {
  element.unsafe_raw_html("", "span", [], html_string)
}

fn raw_div(html_string: String) -> element.Element(a) {
  element.unsafe_raw_html("", "div", [], html_string)
}

fn raw_ul(html_string: String) -> element.Element(a) {
  element.unsafe_raw_html("", "ul", [], html_string)
}

// pub type Page {
//   Home
// }

// pub type Category {
//   Title
// }

pub fn title(l: Lang) -> String {
  case l {
    En -> "D-LAN - A LAN file sharing software"
    Fr -> "D-LAN - Un logiciel de partage de fichiers en LAN"
    De -> "D-LAN - Eine Software zum Dateiaustausch im LAN"
    Es -> "D-LAN - Un software de intercambio de archivos en LAN"
    It -> "D-LAN - Un software di condivisione file in LAN"
    Ru -> "D-LAN - Программа для обмена файлами в локальной сети"
    Ko -> "D-LAN - LAN 파일 공유 소프트웨어"
    Ja -> "D-LAN - LAN ファイル共有ソフトウェア"
  }
}

pub fn header_support_us(l: Lang) -> element.Element(a) {
  case l {
    En -> "support us!"
    Fr -> "soutenez-nous !"
    De -> "unterstützen Sie uns!"
    Es -> "¡apóyenos!"
    It -> "sosteneteci!"
    Ru -> "поддержите нас!"
    Ko -> "후원해 주세요!"
    Ja -> "応援してください！"
  }
  |> html.text
}

pub fn menu_home(l: Lang) -> element.Element(a) {
  case l {
    En -> "HOME"
    Fr -> "HOME"
    De -> "STARTSEITE"
    Es -> "INICIO"
    It -> "HOME"
    Ru -> "ГЛАВНАЯ"
    Ko -> "홈"
    Ja -> "ホーム"
  }
  |> html.text
}

pub fn menu_features(l: Lang) -> element.Element(a) {
  case l {
    En -> "FEATURES"
    Fr -> "FONCTIONNALITÉS"
    De -> "FUNKTIONEN"
    Es -> "CARACTERÍSTICAS"
    It -> "FUNZIONALITÀ"
    Ru -> "ВОЗМОЖНОСТИ"
    Ko -> "기능"
    Ja -> "機能"
  }
  |> html.text
}

pub fn menu_faq(l: Lang) -> element.Element(a) {
  case l {
    En -> "FAQ"
    Fr -> "FAQ"
    De -> "FAQ"
    Es -> "FAQ"
    It -> "FAQ"
    Ru -> "FAQ"
    Ko -> "FAQ"
    Ja -> "FAQ"
  }
  |> html.text
}

pub fn menu_about(l: Lang) -> element.Element(a) {
  case l {
    En -> "ABOUT"
    Fr -> "À PROPOS"
    De -> "ÜBER"
    Es -> "ACERCA DE"
    It -> "INFORMAZIONI"
    Ru -> "О ПРОЕКТЕ"
    Ko -> "소개"
    Ja -> "概要"
  }
  |> html.text
}

pub fn home_title(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "D-LAN - A free <abbr title=\"Local Area Network\">LAN</abbr> file sharing software."
    Fr ->
      "D-LAN - Un logiciel libre de partage de fichiers en <abbr title=\"Local Area Network (Réseau local)\">LAN</abbr>."
    De ->
      "D-LAN - Eine freie Software zum Dateiaustausch im <abbr title=\"Local Area Network (lokales Netzwerk)\">LAN</abbr>."
    Es ->
      "D-LAN - Un software libre de intercambio de archivos en <abbr title=\"Local Area Network (red de área local)\">LAN</abbr>."
    It ->
      "D-LAN - Un software libero di condivisione file in <abbr title=\"Local Area Network (rete locale)\">LAN</abbr>."
    Ru ->
      "D-LAN - Свободная программа для обмена файлами в <abbr title=\"Local Area Network (локальная сеть)\">LAN</abbr>."
    Ko ->
      "D-LAN - 자유로운 <abbr title=\"Local Area Network (근거리 통신망)\">LAN</abbr> 파일 공유 소프트웨어."
    Ja ->
      "D-LAN - 自由な <abbr title=\"Local Area Network (ローカルエリアネットワーク)\">LAN</abbr> ファイル共有ソフトウェア。"
  }
  |> raw_span
}

pub fn home_description(l: Lang, features_url: String) -> element.Element(a) {
  case l {
    En ->
      "The goal is to easily exchange a large amount of data in a local area network environment like a LAN-Party. After you launch D-LAN, you will see all other people and their shared files automatically, without any special configuration. See the <a href=\""
      <> features_url
      <> "\">features list</a> for more information."
    Fr ->
      "Le but est de permettre l'échange massif et facile de fichiers sur un réseau local, par exemple lors d'une LAN-Party. Après avoir lancé D-LAN, les autres personnes présentes sur le réseau sont visibles automatiquement sans aucune configuration particulière. Voir la <a href=\""
      <> features_url
      <> "\">page des fonctionnalités</a> pour plus d'informations."
    De ->
      "Das Ziel ist der einfache Austausch großer Datenmengen in einem lokalen Netzwerk, zum Beispiel auf einer LAN-Party. Nach dem Start von D-LAN werden alle anderen Teilnehmer und deren Freigaben automatisch angezeigt, ganz ohne besondere Konfiguration. Weitere Informationen finden Sie in der <a href=\""
      <> features_url
      <> "\">Funktionsliste</a>."
    Es ->
      "El objetivo es intercambiar fácilmente grandes cantidades de datos en una red de área local, por ejemplo durante una LAN party. Tras iniciar D-LAN, todas las demás personas y sus archivos compartidos aparecen automáticamente, sin ninguna configuración especial. Consulte la <a href=\""
      <> features_url
      <> "\">lista de características</a> para más información."
    It ->
      "L'obiettivo è scambiare facilmente grandi quantità di dati in una rete locale, ad esempio durante una LAN party. Dopo aver avviato D-LAN, tutte le altre persone e le loro condivisioni saranno visibili automaticamente, senza alcuna configurazione particolare. Per maggiori informazioni consultate l'<a href=\""
      <> features_url
      <> "\">elenco delle funzionalità</a>."
    Ru ->
      "Цель — простой обмен большими объёмами данных в локальной сети, например на LAN-party. После запуска D-LAN все остальные участники сети и их общие файлы отображаются автоматически, без какой-либо особой настройки. Подробнее см. <a href=\""
      <> features_url
      <> "\">список возможностей</a>."
    Ko ->
      "LAN 파티와 같은 근거리 네트워크 환경에서 대용량 데이터를 쉽게 교환하는 것이 목표입니다. D-LAN을 실행하면 별도의 설정 없이도 네트워크의 다른 모든 사용자와 그들의 공유 파일이 자동으로 표시됩니다. 자세한 내용은 <a href=\""
      <> features_url
      <> "\">기능 목록</a>을 참고하세요."
    Ja ->
      "LAN パーティーのようなローカルネットワーク環境で、大量のデータを手軽にやり取りすることが目的です。D-LAN を起動すると、特別な設定をしなくてもネットワーク上の他のすべての利用者とその共有ファイルが自動的に表示されます。詳しくは<a href=\""
      <> features_url
      <> "\">機能一覧</a>をご覧ください。"
  }
  |> raw_span
}

pub fn home_warning_beta(
  l: Lang,
  bug_report_url: String,
) -> element.Element(a) {
  case l {
    En ->
      "<em>Warning:</em> The current version of D-LAN is a beta and is only for testing purposes. You can report any defect <a href=\""
      <> bug_report_url
      <> "\">here</a>."
    Fr ->
      "<em>Attention :</em> La version actuelle de D-LAN est une bêta, elle ne doit être utilisée qu'à des fins de tests. Il est possible de rapporter les anomalies rencontrées <a href=\""
      <> bug_report_url
      <> "\">ici</a>."
    De ->
      "<em>Achtung:</em> Die aktuelle Version von D-LAN ist eine Beta und nur für Testzwecke gedacht. Fehler können <a href=\""
      <> bug_report_url
      <> "\">hier</a> gemeldet werden."
    Es ->
      "<em>Atención:</em> la versión actual de D-LAN es una beta y solo debe usarse con fines de prueba. Puede informar de cualquier defecto <a href=\""
      <> bug_report_url
      <> "\">aquí</a>."
    It ->
      "<em>Attenzione:</em> la versione attuale di D-LAN è una beta, da utilizzare solo a scopo di test. Potete segnalare eventuali difetti <a href=\""
      <> bug_report_url
      <> "\">qui</a>."
    Ru ->
      "<em>Внимание:</em> текущая версия D-LAN является бета-версией и предназначена только для тестирования. О найденных ошибках можно сообщить <a href=\""
      <> bug_report_url
      <> "\">здесь</a>."
    Ko ->
      "<em>주의:</em> 현재 버전의 D-LAN은 베타 버전으로, 테스트 용도로만 사용해야 합니다. 발견한 결함은 <a href=\""
      <> bug_report_url
      <> "\">여기</a>에서 신고할 수 있습니다."
    Ja ->
      "<em>注意:</em> 現在のバージョンの D-LAN はベータ版であり、テスト目的のみに使用してください。不具合は<a href=\""
      <> bug_report_url
      <> "\">こちら</a>から報告できます。"
  }
  |> raw_span
}

pub fn features_disclaimer(
  l: Lang,
  planned_features_url: String,
) -> element.Element(a) {
  case l {
    En ->
      "<em>Here are the main features of the current release.</em> D-LAN is constantly under development, you can see <a href=\""
      <> planned_features_url
      <> "\">here</a> the planned features."
    Fr ->
      "<em>Voici la liste des principales fonctionnalités de la version courante.</em> D-LAN est en développement constant, vous pouvez voir <a href=\""
      <> planned_features_url
      <> "\">ici</a> les fonctionnalités planifiées pour les futures versions."
    De ->
      "<em>Hier sind die wichtigsten Funktionen der aktuellen Version.</em> D-LAN wird ständig weiterentwickelt, die geplanten Funktionen können <a href=\""
      <> planned_features_url
      <> "\">hier</a> eingesehen werden."
    Es ->
      "<em>Estas son las principales características de la versión actual.</em> D-LAN está en constante desarrollo; puede ver <a href=\""
      <> planned_features_url
      <> "\">aquí</a> las características planificadas."
    It ->
      "<em>Ecco le principali funzionalità della versione attuale.</em> D-LAN è in costante sviluppo, potete vedere <a href=\""
      <> planned_features_url
      <> "\">qui</a> le funzionalità pianificate."
    Ru ->
      "<em>Вот основные возможности текущей версии.</em> D-LAN постоянно развивается; запланированные возможности можно посмотреть <a href=\""
      <> planned_features_url
      <> "\">здесь</a>."
    Ko ->
      "<em>다음은 현재 버전의 주요 기능입니다.</em> D-LAN은 지속적으로 개발되고 있으며, 계획된 기능은 <a href=\""
      <> planned_features_url
      <> "\">여기</a>에서 확인할 수 있습니다."
    Ja ->
      "<em>以下は現在のバージョンの主な機能です。</em> D-LAN は常に開発が続けられており、予定されている機能は<a href=\""
      <> planned_features_url
      <> "\">こちら</a>で確認できます。"
  }
  |> raw_span
}

pub fn features_feat_1(l: Lang) -> element.Element(a) {
  case l {
    En -> "Share files and folders in a local area network environment (LAN)."
    Fr -> "Partage de fichiers et dossiers sur un réseau local (LAN)."
    De -> "Dateien und Ordner in einem lokalen Netzwerk (LAN) teilen."
    Es -> "Intercambio de archivos y carpetas en una red de área local (LAN)."
    It -> "Condivisione di file e cartelle in una rete locale (LAN)."
    Ru -> "Обмен файлами и папками в локальной сети (LAN)."
    Ko -> "근거리 네트워크(LAN) 환경에서 파일과 폴더를 공유합니다."
    Ja -> "ローカルエリアネットワーク (LAN) 環境でファイルやフォルダーを共有します。"
  }
  |> html.text
}

pub fn features_feat_2(l: Lang) -> element.Element(a) {
  case l {
    En -> "Distributed transfers to increase performance and reliability."
    Fr ->
      "Transferts distribués pour de meilleures performances et une meilleure fiabilité."
    De -> "Verteilte Übertragungen für mehr Leistung und Zuverlässigkeit."
    Es ->
      "Transferencias distribuidas para mejorar el rendimiento y la fiabilidad."
    It -> "Trasferimenti distribuiti per migliorare prestazioni e affidabilità."
    Ru -> "Распределённые передачи для повышения скорости и надёжности."
    Ko -> "분산 전송으로 성능과 안정성을 높입니다."
    Ja -> "分散転送によって性能と信頼性を高めます。"
  }
  |> html.text
}

pub fn features_feat_3(l: Lang) -> element.Element(a) {
  case l {
    En -> "Very easy to use: no configuration, no central server."
    Fr -> "Très facile à utiliser, pas de configuration ni de serveur central."
    De ->
      "Sehr einfach zu benutzen: keine Konfiguration, kein zentraler Server."
    Es -> "Muy fácil de usar: sin configuración y sin servidor central."
    It ->
      "Molto facile da usare: nessuna configurazione, nessun server centrale."
    Ru ->
      "Очень прост в использовании: никакой настройки, никакого центрального сервера."
    Ko -> "매우 쉬운 사용법: 별도의 설정도, 중앙 서버도 필요 없습니다."
    Ja -> "とても簡単に使えます。設定も中央サーバーも不要です。"
  }
  |> html.text
}

pub fn features_feat_4(l: Lang) -> element.Element(a) {
  case l {
    En -> "Fast indexed search among all other peers."
    Fr -> "Recherche indexée rapide parmi l'ensemble des pairs."
    De -> "Schnelle indizierte Suche über alle anderen Peers."
    Es -> "Búsqueda indexada rápida entre todos los demás pares."
    It -> "Ricerca indicizzata veloce tra tutti gli altri peer."
    Ru -> "Быстрый индексированный поиск по всем остальным пирам."
    Ko -> "모든 피어를 대상으로 한 빠른 색인 검색."
    Ja -> "他のすべてのピアを対象とした高速なインデックス検索。"
  }
  |> html.text
}

pub fn features_feat_5(l: Lang) -> element.Element(a) {
  case l {
    En -> "Browse all files and folders of any other peer."
    Fr ->
      "Possibilité de naviguer dans les fichiers et dossiers des autres pairs."
    De -> "Alle Dateien und Ordner jedes anderen Peers durchstöbern."
    Es ->
      "Posibilidad de explorar todos los archivos y carpetas de cualquier otro par."
    It ->
      "Possibilità di esplorare tutti i file e le cartelle degli altri peer."
    Ru -> "Просмотр всех файлов и папок любого другого пира."
    Ko -> "다른 피어의 모든 파일과 폴더를 탐색할 수 있습니다."
    Ja -> "他のピアのすべてのファイルとフォルダーを閲覧できます。"
  }
  |> html.text
}

pub fn features_feat_6(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "Manage the download queue. It includes adding, deleting or reordering."
    Fr ->
      "Gestion d'une liste des transferts. Il est possible d'ajouter, supprimer ou déplacer des transferts."
    De ->
      "Verwaltung der Download-Warteschlange: Hinzufügen, Löschen und Umsortieren."
    Es ->
      "Gestión de la cola de descargas: es posible añadir, eliminar o reordenar transferencias."
    It ->
      "Gestione della coda di download: è possibile aggiungere, eliminare o riordinare i trasferimenti."
    Ru ->
      "Управление очередью загрузок: добавление, удаление и изменение порядка."
    Ko -> "다운로드 대기열 관리: 추가, 삭제, 순서 변경이 가능합니다."
    Ja -> "ダウンロードキューの管理。追加、削除、並べ替えができます。"
  }
  |> html.text
}

pub fn features_feat_7(l: Lang) -> element.Element(a) {
  case l {
    En -> "Global chat and discussion channels."
    Fr -> "Chat global et canaux de discussion."
    De -> "Globaler Chat und Diskussionskanäle."
    Es -> "Chat global y canales de conversación."
    It -> "Chat globale e canali di discussione."
    Ru -> "Общий чат и каналы общения."
    Ko -> "전체 채팅 및 대화 채널."
    Ja -> "全体チャットとチャットチャンネル。"
  }
  |> html.text
}

pub fn features_feat_8(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "D-LAN can run without graphic interface (GUI) and be controlled remotely."
    Fr ->
      "D-LAN peut être lancé sans interface graphique (<i>GUI</i>) et être piloté à distance."
    De ->
      "D-LAN kann ohne grafische Oberfläche (GUI) laufen und ferngesteuert werden."
    Es ->
      "D-LAN puede ejecutarse sin interfaz gráfica (GUI) y controlarse de forma remota."
    It ->
      "D-LAN può funzionare senza interfaccia grafica (GUI) ed essere controllato da remoto."
    Ru ->
      "D-LAN может работать без графического интерфейса (GUI) и управляться удалённо."
    Ko -> "D-LAN은 그래픽 인터페이스(GUI) 없이 실행할 수 있으며 원격으로 제어할 수 있습니다."
    Ja -> "D-LAN はグラフィカルインターフェース (GUI) なしで実行でき、リモートから操作できます。"
  }
  |> raw_span
}

pub fn features_feat_9(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "<a href=\"https://github.com/Ummon/D-LAN\">Open source</a>. Source code is distributed under the GPLv3 license."
    Fr ->
      "<a href=\"https://github.com/Ummon/D-LAN\">Open source</a>. Le code est distribué sous la licence GPLv3."
    De ->
      "<a href=\"https://github.com/Ummon/D-LAN\">Open Source</a>. Der Quellcode wird unter der GPLv3-Lizenz veröffentlicht."
    Es ->
      "<a href=\"https://github.com/Ummon/D-LAN\">Código abierto</a>. El código fuente se distribuye bajo la licencia GPLv3."
    It ->
      "<a href=\"https://github.com/Ummon/D-LAN\">Open source</a>. Il codice sorgente è distribuito sotto licenza GPLv3."
    Ru ->
      "<a href=\"https://github.com/Ummon/D-LAN\">Открытый исходный код</a>. Исходный код распространяется под лицензией GPLv3."
    Ko ->
      "<a href=\"https://github.com/Ummon/D-LAN\">오픈 소스</a>. 소스 코드는 GPLv3 라이선스로 배포됩니다."
    Ja ->
      "<a href=\"https://github.com/Ummon/D-LAN\">オープンソース</a>。ソースコードは GPLv3 ライセンスで配布されています。"
  }
  |> raw_span
}

pub fn features_feat_10(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "Free of any sort of ads or <a href=\"https://en.wikipedia.org/wiki/Malware\">malware</a>."
    Fr ->
      "Ne contient aucune sorte de publicité ou de <a href=\"https://fr.wikipedia.org/wiki/Logiciel_malveillant\">'malware'</a>."
    De ->
      "Frei von jeglicher Werbung und <a href=\"https://de.wikipedia.org/wiki/Schadprogramm\">Schadsoftware</a>."
    Es ->
      "Libre de todo tipo de publicidad y de <a href=\"https://es.wikipedia.org/wiki/Malware\">malware</a>."
    It ->
      "Privo di qualsiasi pubblicità o <a href=\"https://it.wikipedia.org/wiki/Malware\">malware</a>."
    Ru ->
      "Не содержит никакой рекламы и <a href=\"https://ru.wikipedia.org/wiki/%D0%92%D1%80%D0%B5%D0%B4%D0%BE%D0%BD%D0%BE%D1%81%D0%BD%D0%B0%D1%8F_%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B0\">вредоносных программ</a>."
    Ko ->
      "어떠한 광고나 <a href=\"https://ko.wikipedia.org/wiki/%EC%95%85%EC%84%B1_%EC%86%8C%ED%94%84%ED%8A%B8%EC%9B%A8%EC%96%B4\">악성 소프트웨어</a>도 포함하지 않습니다."
    Ja ->
      "いかなる広告や<a href=\"https://ja.wikipedia.org/wiki/%E3%83%9E%E3%83%AB%E3%82%A6%E3%82%A7%E3%82%A2\">マルウェア</a>も含みません。"
  }
  |> raw_span
}

pub fn features_help_us(l: Lang, support_url: String) -> element.Element(a) {
  case l {
    En ->
      "Don't forget to <a href=\""
      <> support_url
      <> "\">support us</a>. It will help to maintain and add new features."
    Fr ->
      "N'oubliez pas de nous <a href=\""
      <> support_url
      <> "\">aider</a>, cela permettra la maintenance et l'ajout de nouvelles fonctionnalités."
    De ->
      "Vergessen Sie nicht, uns zu <a href=\""
      <> support_url
      <> "\">unterstützen</a>. Das hilft bei der Wartung und der Entwicklung neuer Funktionen."
    Es ->
      "No olvide <a href=\""
      <> support_url
      <> "\">apoyarnos</a>: eso ayudará al mantenimiento y a la incorporación de nuevas características."
    It ->
      "Non dimenticate di <a href=\""
      <> support_url
      <> "\">sostenerci</a>: ciò aiuterà la manutenzione e l'aggiunta di nuove funzionalità."
    Ru ->
      "Не забудьте <a href=\""
      <> support_url
      <> "\">поддержать нас</a> — это поможет в сопровождении и добавлении новых возможностей."
    Ko ->
      "<a href=\""
      <> support_url
      <> "\">후원</a>도 잊지 마세요. 유지 보수와 새로운 기능 추가에 도움이 됩니다."
    Ja -> "<a href=\"" <> support_url <> "\">支援</a>もお忘れなく。保守や新機能の追加に役立ちます。"
  }
  |> raw_span
}

pub fn faq_q1(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "What are the benefits of using D-LAN instead of the default system tools?"
    Fr ->
      "Quels sont les avantages d'utiliser D-LAN à la place des outils par défaut du système ?"
    De ->
      "Welche Vorteile bietet D-LAN gegenüber den Standardwerkzeugen des Systems?"
    Es ->
      "¿Qué ventajas tiene usar D-LAN en lugar de las herramientas predeterminadas del sistema?"
    It ->
      "Quali sono i vantaggi di usare D-LAN rispetto agli strumenti predefiniti del sistema?"
    Ru ->
      "Какие преимущества даёт D-LAN по сравнению со стандартными средствами системы?"
    Ko -> "시스템 기본 도구 대신 D-LAN을 사용하면 어떤 이점이 있나요?"
    Ja -> "システム標準のツールの代わりに D-LAN を使う利点は何ですか？"
  }
  |> html.text
}

pub fn faq_a1(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "D-LAN is designed for massive transfers, you can manage a queue of files to be downloaded. A file may be downloaded automatically from many peers at the same time to speed up the transfer and cope with peer downtime. D-LAN has a fast global search feature that the default system file sharing doesn't have. You will find more information from the <a href=\"features.html\">feature page</a>."
    Fr ->
      "D-LAN est conçu pour des transferts massifs, il est possible de gérer une liste des fichiers à transférer. Un fichier peut être transféré depuis plusieurs pairs simultanément pour augmenter la vitesse ainsi que la fiabilité. Il est possible d'effectuer une recherche globale, ce que les partages système par défaut n'ont pas. Pour plus d'informations voir la <a href=\"features.html\">page des fonctionnalités</a>."
    De ->
      "D-LAN ist für massive Übertragungen ausgelegt: Es lässt sich eine Warteschlange der herunterzuladenden Dateien verwalten. Eine Datei kann automatisch von mehreren Peers gleichzeitig heruntergeladen werden, was die Übertragung beschleunigt und den Ausfall einzelner Peers ausgleicht. D-LAN bietet zudem eine schnelle globale Suche, die die Standard-Dateifreigabe des Systems nicht hat. Weitere Informationen finden Sie auf der <a href=\"features.html\">Funktionsseite</a>."
    Es ->
      "D-LAN está diseñado para transferencias masivas: es posible gestionar una cola de archivos por descargar. Un archivo puede descargarse automáticamente desde varios pares a la vez, lo que acelera la transferencia y compensa la desconexión de un par. D-LAN también dispone de una búsqueda global rápida, algo que el intercambio de archivos predeterminado del sistema no tiene. Encontrará más información en la <a href=\"features.html\">página de características</a>."
    It ->
      "D-LAN è progettato per trasferimenti massicci: è possibile gestire una coda di file da scaricare. Un file può essere scaricato automaticamente da più peer contemporaneamente, per accelerare il trasferimento e sopperire alla disconnessione di un peer. D-LAN dispone inoltre di una ricerca globale veloce, assente nella condivisione file predefinita del sistema. Trovate maggiori informazioni nella <a href=\"features.html\">pagina delle funzionalità</a>."
    Ru ->
      "D-LAN рассчитан на массовые передачи: можно управлять очередью файлов для загрузки. Файл может автоматически загружаться с нескольких пиров одновременно, что ускоряет передачу и компенсирует отключение пира. Кроме того, в D-LAN есть быстрый глобальный поиск, которого нет в стандартном общем доступе к файлам. Подробнее см. <a href=\"features.html\">страницу возможностей</a>."
    Ko ->
      "D-LAN은 대량 전송을 위해 설계되어 다운로드할 파일 대기열을 관리할 수 있습니다. 파일은 여러 피어로부터 동시에 자동으로 다운로드되어 전송 속도가 빨라지고 피어의 접속 종료에도 대비할 수 있습니다. 또한 시스템 기본 파일 공유에는 없는 빠른 전체 검색 기능이 있습니다. 자세한 내용은 <a href=\"features.html\">기능 페이지</a>를 참고하세요."
    Ja ->
      "D-LAN は大量の転送のために設計されており、ダウンロードするファイルのキューを管理できます。ファイルは複数のピアから同時に自動でダウンロードされるため、転送が速くなり、ピアの切断にも備えられます。また、システム標準のファイル共有にはない高速な全体検索機能があります。詳しくは<a href=\"features.html\">機能ページ</a>をご覧ください。"
  }
  |> raw_span
}

pub fn faq_q2(l: Lang) -> element.Element(a) {
  case l {
    En -> "I don't see other computers in my network."
    Fr -> "Je ne vois pas les autres ordinateurs de mon réseau."
    De -> "Ich sehe die anderen Computer in meinem Netzwerk nicht."
    Es -> "No veo los demás equipos de mi red."
    It -> "Non vedo gli altri computer della mia rete."
    Ru -> "Я не вижу другие компьютеры в своей сети."
    Ko -> "네트워크에서 다른 컴퓨터가 보이지 않습니다."
    Ja -> "ネットワーク上の他のコンピューターが見えません。"
  }
  |> html.text
}

pub fn faq_a2(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "<ul>"
      <> "<li>Check you have the latest version of D-LAN.</li>"
      <> "<li>Be sure the ports 59486 (<i>UDP</i>) and 59487 (<i>UDP + TCP</i>) are opened in your firewall.</li>"
      <> "<li>Be sure UDP multicast is allowed in your network. The address used is this one: 236.13.43.24.</li>"
      <> "<li>All peers must use the same protocol: IPv4 or IPv6, look on <i>Settings &gt; Network</i>.</li>"
      <> "</ul>"
    Fr ->
      "<ul>"
      <> "<li>Vérifier que la dernière version de D-LAN est installée.</li>"
      <> "<li>S'assurer que les ports 59486 (<i>UDP</i>) et 59487 (<i>UDP + TCP</i>) sont ouverts sur le pare-feu (<i>firewall</i>).</li>"
      <> "<li>S'assurer que l'UDP multicast est autorisé sur le réseau. L'adresse utilisée est la suivante : 236.13.43.24.</li>"
      <> "<li>Tous les pairs doivent utiliser le même protocole : IPv4 ou IPv6, voir <i>Paramètres &gt; Réseau</i>.</li>"
      <> "</ul>"
    De ->
      "<ul>"
      <> "<li>Stellen Sie sicher, dass die neueste Version von D-LAN installiert ist.</li>"
      <> "<li>Stellen Sie sicher, dass die Ports 59486 (<i>UDP</i>) und 59487 (<i>UDP + TCP</i>) in Ihrer Firewall geöffnet sind.</li>"
      <> "<li>Stellen Sie sicher, dass UDP-Multicast in Ihrem Netzwerk erlaubt ist. Die verwendete Adresse lautet: 236.13.43.24.</li>"
      <> "<li>Alle Peers müssen dasselbe Protokoll verwenden: IPv4 oder IPv6, siehe <i>Einstellungen &gt; Netzwerk</i>.</li>"
      <> "</ul>"
    Es ->
      "<ul>"
      <> "<li>Compruebe que tiene la última versión de D-LAN.</li>"
      <> "<li>Asegúrese de que los puertos 59486 (<i>UDP</i>) y 59487 (<i>UDP + TCP</i>) están abiertos en su cortafuegos.</li>"
      <> "<li>Asegúrese de que la multidifusión UDP está permitida en su red. La dirección utilizada es: 236.13.43.24.</li>"
      <> "<li>Todos los pares deben usar el mismo protocolo: IPv4 o IPv6, véase <i>Configuración &gt; Red</i>.</li>"
      <> "</ul>"
    It ->
      "<ul>"
      <> "<li>Verificate di avere l'ultima versione di D-LAN.</li>"
      <> "<li>Assicuratevi che le porte 59486 (<i>UDP</i>) e 59487 (<i>UDP + TCP</i>) siano aperte nel vostro firewall.</li>"
      <> "<li>Assicuratevi che il multicast UDP sia consentito nella vostra rete. L'indirizzo utilizzato è: 236.13.43.24.</li>"
      <> "<li>Tutti i peer devono usare lo stesso protocollo: IPv4 o IPv6, vedere <i>Impostazioni &gt; Rete</i>.</li>"
      <> "</ul>"
    Ru ->
      "<ul>"
      <> "<li>Убедитесь, что у вас установлена последняя версия D-LAN.</li>"
      <> "<li>Убедитесь, что порты 59486 (<i>UDP</i>) и 59487 (<i>UDP + TCP</i>) открыты в вашем брандмауэре.</li>"
      <> "<li>Убедитесь, что в вашей сети разрешён UDP multicast. Используемый адрес: 236.13.43.24.</li>"
      <> "<li>Все пиры должны использовать один и тот же протокол: IPv4 или IPv6, см. <i>Настройки &gt; Сеть</i>.</li>"
      <> "</ul>"
    Ko ->
      "<ul>"
      <> "<li>최신 버전의 D-LAN이 설치되어 있는지 확인하세요.</li>"
      <> "<li>방화벽에서 59486 (<i>UDP</i>) 및 59487 (<i>UDP + TCP</i>) 포트가 열려 있는지 확인하세요.</li>"
      <> "<li>네트워크에서 UDP 멀티캐스트가 허용되어 있는지 확인하세요. 사용되는 주소는 236.13.43.24입니다.</li>"
      <> "<li>모든 피어는 동일한 프로토콜(IPv4 또는 IPv6)을 사용해야 합니다. <i>설정 &gt; 네트워크</i>를 확인하세요.</li>"
      <> "</ul>"
    Ja ->
      "<ul>"
      <> "<li>D-LAN が最新バージョンであることを確認してください。</li>"
      <> "<li>ファイアウォールでポート 59486 (<i>UDP</i>) と 59487 (<i>UDP + TCP</i>) が開いていることを確認してください。</li>"
      <> "<li>ネットワークで UDP マルチキャストが許可されていることを確認してください。使用されるアドレスは 236.13.43.24 です。</li>"
      <> "<li>すべてのピアが同じプロトコル (IPv4 または IPv6) を使用する必要があります。<i>設定 &gt; ネットワーク</i>を確認してください。</li>"
      <> "</ul>"
  }
  |> raw_div
}

pub fn faq_q3(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "D-LAN slows down my computer when hashing. What is that and why is it needed?"
    Fr ->
      "D-LAN ralentit mon ordinateur lors du calcul des empreintes, qu'est-ce que c'est et pourquoi est-ce nécessaire ?"
    De ->
      "D-LAN verlangsamt meinen Computer beim Berechnen der Hashes. Was ist das und wozu ist es nötig?"
    Es ->
      "D-LAN ralentiza mi equipo al calcular los hashes, ¿qué son y por qué son necesarios?"
    It ->
      "D-LAN rallenta il mio computer durante il calcolo degli hash: cosa sono e perché sono necessari?"
    Ru ->
      "D-LAN замедляет мой компьютер при вычислении хешей. Что это такое и зачем это нужно?"
    Ko -> "해시 계산 중에 D-LAN이 컴퓨터를 느리게 합니다. 해시란 무엇이고 왜 필요한가요?"
    Ja -> "ハッシュの計算中に D-LAN がコンピューターを遅くします。ハッシュとは何で、なぜ必要なのですか？"
  }
  |> html.text
}

pub fn faq_a3(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "Hashes are necessary to identify all the parts of a file. They allow multi-source downloading and data integrity verification. During the hashing process only one core is used, and almost all computers sold today are multi-core. All the shared files have to be read once, so it can slow down disk access a bit."
    Fr ->
      "Les empreintes servent à identifier les données des fichiers. Cela permet de télécharger un fichier chez plusieurs pairs simultanément. "
      <> "Le calcul des empreintes n'utilise pas plus d'un cœur du processeur (la plupart des processeurs actuels sont multicœurs) "
      <> "mais doit lire toutes les données partagées ce qui peut ralentir un peu l'accès au disque dur."
    De ->
      "Hashes sind nötig, um alle Teile einer Datei zu identifizieren. Sie ermöglichen das Herunterladen aus mehreren Quellen sowie die Überprüfung der Datenintegrität. "
      <> "Während der Berechnung wird nur ein Prozessorkern verwendet, und fast alle heute verkauften Computer haben mehrere Kerne. "
      <> "Alle freigegebenen Dateien müssen einmal gelesen werden, was den Festplattenzugriff etwas verlangsamen kann."
    Es ->
      "Los hashes son necesarios para identificar todas las partes de un archivo. Permiten la descarga desde múltiples fuentes y la verificación de la integridad de los datos. "
      <> "Durante el cálculo solo se usa un núcleo, y casi todos los equipos que se venden hoy son multinúcleo. "
      <> "Todos los archivos compartidos deben leerse una vez, lo que puede ralentizar un poco el acceso al disco."
    It ->
      "Gli hash sono necessari per identificare tutte le parti di un file. Permettono il download da più fonti e la verifica dell'integrità dei dati. "
      <> "Durante il calcolo degli hash viene usato un solo core, e quasi tutti i computer venduti oggi sono multi-core. "
      <> "Tutti i file condivisi devono essere letti una volta, il che può rallentare un po' l'accesso al disco."
    Ru ->
      "Хеши необходимы для идентификации всех частей файла. Они позволяют загружать файл из нескольких источников и проверять целостность данных. "
      <> "При вычислении хешей используется только одно ядро процессора, а почти все продаваемые сегодня компьютеры многоядерные. "
      <> "Все общие файлы должны быть прочитаны один раз, что может немного замедлить доступ к диску."
    Ko ->
      "해시는 파일의 각 부분을 식별하는 데 필요합니다. 이를 통해 다중 소스 다운로드와 데이터 무결성 검증이 가능합니다. "
      <> "해시 계산에는 코어 하나만 사용되며, 요즘 판매되는 컴퓨터는 대부분 멀티 코어입니다. "
      <> "공유되는 모든 파일을 한 번씩 읽어야 하므로 디스크 접근이 다소 느려질 수 있습니다."
    Ja ->
      "ハッシュはファイルのすべての部分を識別するために必要です。これにより複数のソースからのダウンロードとデータの整合性の検証が可能になります。"
      <> "ハッシュの計算にはコアが 1 つしか使われませんが、今日販売されているコンピューターのほとんどはマルチコアです。"
      <> "共有するすべてのファイルを一度読み込む必要があるため、ディスクアクセスが少し遅くなることがあります。"
  }
  |> html.text
}

pub fn faq_q4(l: Lang) -> element.Element(a) {
  case l {
    En -> "D-LAN uses too many resources, how can I improve that?"
    Fr ->
      "D-LAN utilise trop de ressource, comment puis-je faire pour réduire sa consommation ?"
    De -> "D-LAN verbraucht zu viele Ressourcen, was kann ich dagegen tun?"
    Es -> "D-LAN consume demasiados recursos, ¿cómo puedo mejorarlo?"
    It -> "D-LAN usa troppe risorse, come posso migliorare la situazione?"
    Ru -> "D-LAN использует слишком много ресурсов, как это исправить?"
    Ko -> "D-LAN이 리소스를 너무 많이 사용합니다. 어떻게 개선할 수 있나요?"
    Ja -> "D-LAN がリソースを使いすぎます。どうすれば改善できますか？"
  }
  |> html.text
}

pub fn faq_a4(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "<p>If D-LAN is currently computing some hashes, you have to wait for the end of this operation, see the previous question. Otherwise it's possible to close the main D-LAN window, the GUI will be disconnected from the core and take less resources.</p><p>Try to not have more than 10,000 files in queue, periodically clear the queue.</p>"
    Fr ->
      "<p>Si D-LAN est en train de calculer les empreintes, il faut attendre que l'opération soit terminée, voir la question précédente. "
      <> "Sinon il est possible de fermer la fenêtre de D-LAN, l'interface est alors déconnectée du cœur et consomme moins de ressources.</p><p>Éviter d'avoir plus de 10 000 fichiers en queue, périodiquement enlever les fichiers complets.</p>"
    De ->
      "<p>Wenn D-LAN gerade Hashes berechnet, müssen Sie das Ende dieses Vorgangs abwarten, siehe die vorherige Frage. "
      <> "Ansonsten kann das Hauptfenster von D-LAN geschlossen werden: Die Oberfläche wird dann vom Kern getrennt und verbraucht weniger Ressourcen.</p><p>Vermeiden Sie mehr als 10.000 Dateien in der Warteschlange und leeren Sie diese regelmäßig.</p>"
    Es ->
      "<p>Si D-LAN está calculando hashes, debe esperar a que termine la operación, véase la pregunta anterior. "
      <> "En caso contrario, es posible cerrar la ventana principal de D-LAN: la interfaz se desconectará del núcleo y consumirá menos recursos.</p><p>Procure no tener más de 10.000 archivos en cola y límpiela periódicamente.</p>"
    It ->
      "<p>Se D-LAN sta calcolando degli hash, bisogna attendere la fine dell'operazione, vedere la domanda precedente. "
      <> "Altrimenti è possibile chiudere la finestra principale di D-LAN: l'interfaccia verrà disconnessa dal core e userà meno risorse.</p><p>Cercate di non avere più di 10.000 file in coda e svuotatela periodicamente.</p>"
    Ru ->
      "<p>Если D-LAN в данный момент вычисляет хеши, нужно дождаться окончания этой операции, см. предыдущий вопрос. "
      <> "В остальных случаях можно закрыть главное окно D-LAN: интерфейс отключится от ядра и будет потреблять меньше ресурсов.</p><p>Старайтесь не держать в очереди более 10 000 файлов и периодически очищайте её.</p>"
    Ko ->
      "<p>D-LAN이 해시를 계산 중이라면 이 작업이 끝날 때까지 기다려야 합니다(이전 질문 참고). "
      <> "그 외의 경우 D-LAN 메인 창을 닫으면 GUI가 코어에서 분리되어 리소스를 덜 사용합니다.</p><p>대기열에 10,000개 이상의 파일을 두지 않도록 하고, 주기적으로 대기열을 정리하세요.</p>"
    Ja ->
      "<p>D-LAN がハッシュを計算している場合は、その処理が終わるまで待つ必要があります。前の質問を参照してください。"
      <> "そうでない場合は D-LAN のメインウィンドウを閉じることができます。GUI がコアから切り離され、消費するリソースが少なくなります。</p><p>キューに 10,000 個以上のファイルを入れないようにし、定期的にキューを整理してください。</p>"
  }
  |> raw_div
}

pub fn faq_q5(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "Can I configure D-LAN to start automatically when my computer starts?"
    Fr ->
      "Est-il possible de configurer D-LAN pour qu'il démarre automatiquement au démarrage de la machine ?"
    De ->
      "Kann D-LAN so konfiguriert werden, dass es beim Hochfahren des Computers automatisch startet?"
    Es ->
      "¿Puedo configurar D-LAN para que se inicie automáticamente al arrancar el equipo?"
    It ->
      "Posso configurare D-LAN perché si avvii automaticamente all'accensione del computer?"
    Ru ->
      "Можно ли настроить D-LAN так, чтобы он запускался автоматически при включении компьютера?"
    Ko -> "컴퓨터가 시작될 때 D-LAN이 자동으로 실행되도록 설정할 수 있나요?"
    Ja -> "コンピューターの起動時に D-LAN が自動的に起動するように設定できますか？"
  }
  |> html.text
}

pub fn faq_a5(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "<i>Windows 7</i>: Go to <i>Control Panel</i> &gt; <i>Administrative Tools</i> &gt; <i>Services</i>. Open the properties of <i>D-LAN Core</i> and set the <i>Startup type</i> from <i>Manual</i> to <i>Automatic</i>."
    Fr ->
      "<i>Windows 7</i> : Allez dans <i>Panneau de configuration</i> &gt; <i>Outils d'administration</i> &gt; <i>Services</i>. Ouvrir la fenêtre de propriétés de <i>D-LAN Core</i> et définir le <i>Type de Démarrage</i> à <i>Automatique</i>."
    De ->
      "<i>Windows 7</i>: Gehen Sie zu <i>Systemsteuerung</i> &gt; <i>Verwaltung</i> &gt; <i>Dienste</i>. Öffnen Sie die Eigenschaften von <i>D-LAN Core</i> und stellen Sie den <i>Starttyp</i> von <i>Manuell</i> auf <i>Automatisch</i>."
    Es ->
      "<i>Windows 7</i>: vaya a <i>Panel de control</i> &gt; <i>Herramientas administrativas</i> &gt; <i>Servicios</i>. Abra las propiedades de <i>D-LAN Core</i> y cambie el <i>Tipo de inicio</i> de <i>Manual</i> a <i>Automático</i>."
    It ->
      "<i>Windows 7</i>: andate in <i>Pannello di controllo</i> &gt; <i>Strumenti di amministrazione</i> &gt; <i>Servizi</i>. Aprite le proprietà di <i>D-LAN Core</i> e impostate il <i>Tipo di avvio</i> da <i>Manuale</i> ad <i>Automatico</i>."
    Ru ->
      "<i>Windows 7</i>: откройте <i>Панель управления</i> &gt; <i>Администрирование</i> &gt; <i>Службы</i>. Откройте свойства <i>D-LAN Core</i> и измените <i>Тип запуска</i> с <i>Вручную</i> на <i>Автоматически</i>."
    Ko ->
      "<i>Windows 7</i>: <i>제어판</i> &gt; <i>관리 도구</i> &gt; <i>서비스</i>로 이동합니다. <i>D-LAN Core</i>의 속성을 열고 <i>시작 유형</i>을 <i>수동</i>에서 <i>자동</i>으로 변경하세요."
    Ja ->
      "<i>Windows 7</i>: <i>コントロールパネル</i> &gt; <i>管理ツール</i> &gt; <i>サービス</i> を開きます。<i>D-LAN Core</i> のプロパティを開き、<i>スタートアップの種類</i>を<i>手動</i>から<i>自動</i>に変更します。"
  }
  |> raw_span
}

pub fn about_author(l: Lang, name: element.Element(a)) -> element.Element(a) {
  element.fragment([
    case l {
      En -> "Author : "
      Fr -> "Auteur : "
      De -> "Autor: "
      Es -> "Autor: "
      It -> "Autore: "
      Ru -> "Автор: "
      Ko -> "제작자: "
      Ja -> "作者: "
    }
      |> html.text,
    name,
  ])
}

pub fn about_linux(l: Lang, name: element.Element(a)) -> element.Element(a) {
  element.fragment([
    case l {
      En -> "Linux maintainer : "
      Fr -> "Responsable Linux : "
      De -> "Linux-Betreuer: "
      Es -> "Responsable de Linux: "
      It -> "Responsabile Linux: "
      Ru -> "Сопровождающий Linux: "
      Ko -> "Linux 담당자: "
      Ja -> "Linux メンテナー: "
    }
      |> html.text,
    name,
  ])
}

pub fn about_thanks(
  l: Lang,
  name_1: element.Element(a),
  name_2: element.Element(a),
) -> element.Element(a) {
  case l {
    En -> [
      html.text("Thanks to "),
      name_1,
      html.text(" and "),
      name_2,
      html.text(" for their support."),
    ]
    Fr -> [
      html.text("Merci à "),
      name_1,
      html.text(" et "),
      name_2,
      html.text(" pour leur soutien."),
    ]
    De -> [
      html.text("Dank an "),
      name_1,
      html.text(" und "),
      name_2,
      html.text(" für ihre Unterstützung."),
    ]
    Es -> [
      html.text("Gracias a "),
      name_1,
      html.text(" y "),
      name_2,
      html.text(" por su apoyo."),
    ]
    It -> [
      html.text("Grazie a "),
      name_1,
      html.text(" e "),
      name_2,
      html.text(" per il loro supporto."),
    ]
    Ru -> [
      html.text("Спасибо "),
      name_1,
      html.text(" и "),
      name_2,
      html.text(" за их поддержку."),
    ]
    Ko -> [
      html.text("지원해 주신 "),
      name_1,
      html.text("님과 "),
      name_2,
      html.text("님께 감사드립니다."),
    ]
    Ja -> [
      html.text("ご支援いただいた "),
      name_1,
      html.text(" さんと "),
      name_2,
      html.text(" さんに感謝します。"),
    ]
  }
  |> element.fragment
}

pub fn about_tech(l: Lang) -> element.Element(a) {
  case l {
    En -> "Technologies and software used"
    Fr -> "Technologies et logiciels utilisés"
    De -> "Verwendete Technologien und Software"
    Es -> "Tecnologías y software utilizados"
    It -> "Tecnologie e software utilizzati"
    Ru -> "Используемые технологии и программы"
    Ko -> "사용된 기술 및 소프트웨어"
    Ja -> "使用している技術とソフトウェア"
  }
  |> html.text
}

pub fn about_tech_used_d_lan_title(l: Lang) -> element.Element(a) {
  case l {
    En -> "D-LAN"
    Fr -> "D-LAN"
    De -> "D-LAN"
    Es -> "D-LAN"
    It -> "D-LAN"
    Ru -> "D-LAN"
    Ko -> "D-LAN"
    Ja -> "D-LAN"
  }
  |> html.text
}

pub fn about_tech_used_d_lan(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "<li>Programming language: <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>Framework and libraries: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>Compiler: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>Message serializer: <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>Cryptographic hash function: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>Database: <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
    Fr ->
      "<li>Langage de programmation : <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>Bibliothèque logicielle principale : <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>Compilateur : <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>Sérialisation des messages : <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>Fonction de hashage : <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>Base de données : <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
    De ->
      "<li>Programmiersprache: <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>Framework und Bibliotheken: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>Compiler: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>Nachrichten-Serialisierung: <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>Kryptografische Hashfunktion: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>Datenbank: <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
    Es ->
      "<li>Lenguaje de programación: <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>Framework y bibliotecas: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>Compilador: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>Serialización de mensajes: <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>Función hash criptográfica: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>Base de datos: <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
    It ->
      "<li>Linguaggio di programmazione: <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>Framework e librerie: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>Compilatore: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>Serializzazione dei messaggi: <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>Funzione di hash crittografica: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>Database: <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
    Ru ->
      "<li>Язык программирования: <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>Фреймворк и библиотеки: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>Компилятор: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>Сериализация сообщений: <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>Криптографическая хеш-функция: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>База данных: <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
    Ko ->
      "<li>프로그래밍 언어: <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>프레임워크 및 라이브러리: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>컴파일러: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>메시지 직렬화: <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>암호화 해시 함수: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>데이터베이스: <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
    Ja ->
      "<li>プログラミング言語: <a href=\"https://en.wikipedia.org/wiki/C%2B%2B\">C++</a></li>"
      <> "<li>フレームワークとライブラリ: <a href=\"https://www.qt.io/development/qt-framework\">Qt 6</a></li>"
      <> "<li>コンパイラ: <a href=\"https://clang.llvm.org/\">Clang</a></li>"
      <> "<li>メッセージのシリアライズ: <a href=\"https://protobuf.dev/\">Protocol Buffers</a></li>"
      <> "<li>暗号学的ハッシュ関数: <a href=\"https://github.com/BLAKE3-team/BLAKE3\">BLAKE3</a></li>"
      <> "<li>データベース: <a href=\"https://www.sqlite.org/\">SQLite</a></li>"
  }
  |> raw_ul
}

pub fn about_tech_used_tools_title(l: Lang) -> element.Element(a) {
  case l {
    En -> "Development tools"
    Fr -> "Outils de développement"
    De -> "Entwicklungswerkzeuge"
    Es -> "Herramientas de desarrollo"
    It -> "Strumenti di sviluppo"
    Ru -> "Инструменты разработки"
    Ko -> "개발 도구"
    Ja -> "開発ツール"
  }
  |> html.text
}

pub fn about_tech_used_tools(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "<li>Development environment: <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>Version control system: <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>Scripting: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>Project management: <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>Documentation generator: <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>Setup builder: <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>Vector and bitmap graphics editor: <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
    Fr ->
      "<li>Environnement de développement : <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>Système de gestion de versions : <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>Scripting : <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>Gestion de projet : <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>Génération de la documentation : <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>Système d'installation : <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>Éditeur vectoriel et bitmap : <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
    De ->
      "<li>Entwicklungsumgebung: <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>Versionsverwaltung: <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>Skripting: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>Projektverwaltung: <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>Dokumentationsgenerator: <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>Installationsprogramm: <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>Vektor- und Bitmap-Grafikeditor: <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
    Es ->
      "<li>Entorno de desarrollo: <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>Sistema de control de versiones: <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>Scripting: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>Gestión de proyectos: <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>Generador de documentación: <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>Creador del instalador: <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>Editor de gráficos vectoriales y de mapa de bits: <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
    It ->
      "<li>Ambiente di sviluppo: <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>Sistema di controllo versione: <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>Scripting: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>Gestione del progetto: <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>Generatore di documentazione: <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>Creazione dell'installer: <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>Editor di grafica vettoriale e bitmap: <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
    Ru ->
      "<li>Среда разработки: <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>Система контроля версий: <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>Скрипты: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>Управление проектом: <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>Генератор документации: <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>Создание установщика: <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>Редактор векторной и растровой графики: <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
    Ko ->
      "<li>개발 환경: <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>버전 관리 시스템: <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>스크립팅: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>프로젝트 관리: <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>문서 생성기: <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>설치 프로그램 제작: <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>벡터 및 비트맵 그래픽 편집기: <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
    Ja ->
      "<li>開発環境: <a href=\"https://www.qt.io/development/tools/qt-creator-ide\">Qt Creator</a></li>"
      <> "<li>バージョン管理システム: <a href=\"https://git-scm.com/\">git</a></li>"
      <> "<li>スクリプト: <a href=\"https://www.nushell.sh/\">Nushell</a></li>"
      <> "<li>プロジェクト管理: <a href=\"https://www.redmine.org/\">Redmine</a></li>"
      <> "<li>ドキュメント生成: <a href=\"https://www.doxygen.org/\">Doxygen</a></li>"
      <> "<li>インストーラー作成: <a href=\"https://www.jrsoftware.org/isinfo.php\">Inno Setup</a></li>"
      <> "<li>ベクター画像・ビットマップ画像編集: <a href=\"https://www.affinity.studio/\">Affinity</a></li>"
  }
  |> raw_ul
}

pub fn about_tech_used_website_title(l: Lang) -> element.Element(a) {
  case l {
    En -> "Web site"
    Fr -> "Site web"
    De -> "Webseite"
    Es -> "Sitio web"
    It -> "Sito web"
    Ru -> "Веб-сайт"
    Ko -> "웹 사이트"
    Ja -> "ウェブサイト"
  }
  |> html.text
}

pub fn about_tech_used_website(l: Lang) -> element.Element(a) {
  case l {
    En ->
      "<li>Document structure: <a href=\"https://www.w3.org/TR/html5/\">HTML5</a></li>"
      <> "<li>Document presentation: <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>Client side dynamic language: <a href=\"https://en.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
      <> "<li>JavaScript libraries: <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>Server side language: <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>Web server: <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
    Fr ->
      "<li>Structure : <a href=\"https://www.w3.org/TR/html5/\">HTML 5</a></li>"
      <> "<li>Présentation : <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>Langage dynamique côté client : <a href=\"https://fr.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
      <> "<li>Bibliothèques JavaScript : <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>Langage côté serveur : <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>Serveur web : <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
    De ->
      "<li>Dokumentstruktur: <a href=\"https://www.w3.org/TR/html5/\">HTML5</a></li>"
      <> "<li>Dokumentdarstellung: <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>Clientseitige dynamische Sprache: <a href=\"https://de.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
      <> "<li>JavaScript-Bibliotheken: <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>Serverseitige Sprache: <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>Webserver: <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
    Es ->
      "<li>Estructura del documento: <a href=\"https://www.w3.org/TR/html5/\">HTML5</a></li>"
      <> "<li>Presentación del documento: <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>Lenguaje dinámico del lado del cliente: <a href=\"https://es.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
      <> "<li>Bibliotecas JavaScript: <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>Lenguaje del lado del servidor: <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>Servidor web: <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
    It ->
      "<li>Struttura del documento: <a href=\"https://www.w3.org/TR/html5/\">HTML5</a></li>"
      <> "<li>Presentazione del documento: <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>Linguaggio dinamico lato client: <a href=\"https://it.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
      <> "<li>Librerie JavaScript: <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>Linguaggio lato server: <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>Server web: <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
    Ru ->
      "<li>Структура документа: <a href=\"https://www.w3.org/TR/html5/\">HTML5</a></li>"
      <> "<li>Оформление документа: <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>Динамический язык на стороне клиента: <a href=\"https://ru.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
      <> "<li>Библиотеки JavaScript: <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>Язык на стороне сервера: <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>Веб-сервер: <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
    Ko ->
      "<li>문서 구조: <a href=\"https://www.w3.org/TR/html5/\">HTML5</a></li>"
      <> "<li>문서 표현: <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>클라이언트 측 동적 언어: <a href=\"https://ko.wikipedia.org/wiki/자바스크립트\">JavaScript</a></li>"
      <> "<li>JavaScript 라이브러리: <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>서버 측 언어: <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>웹 서버: <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
    Ja ->
      "<li>文書構造: <a href=\"https://www.w3.org/TR/html5/\">HTML5</a></li>"
      <> "<li>文書表現: <a href=\"https://www.w3.org/Style/CSS/current-work\">CSS 3</a> + <a href=\"https://sass-lang.com\">Sass</a></li>"
      <> "<li>クライアント側の動的言語: <a href=\"https://ja.wikipedia.org/wiki/JavaScript\">JavaScript</a></li>"
      <> "<li>JavaScript ライブラリ: <a href=\"https://jquery.com/\">JQuery</a> + <a href=\"https://www.jacklmoore.com/colorbox/\">ColorBox</a></li>"
      <> "<li>サーバー側の言語: <a href=\"https://gleam.run/\">Gleam</a></li>"
      <> "<li>ウェブサーバー: <a href=\"https://gleam-wisp.github.io/wisp/\">Wisp</a> + <a href=\"https://hexdocs.pm/mist/\">Mist</a></li>"
  }
  |> raw_ul
}

pub fn donate_title(l: Lang) -> element.Element(a) {
  case l {
    En -> "Support us"
    Fr -> "Soutenez-nous"
    De -> "Unterstützen Sie uns"
    Es -> "Apóyenos"
    It -> "Sosteneteci"
    Ru -> "Поддержите нас"
    Ko -> "후원하기"
    Ja -> "支援する"
  }
  |> html.text
}

pub fn donate_intro(l: Lang) -> element.Element(a) {
  case l {
    En -> "If you like this project and want to see it grow, support us!"
    Fr ->
      "Si vous aimez ce projet et que vous voulez le voir grandir, soutenez-nous !"
    De ->
      "Wenn Ihnen dieses Projekt gefällt und Sie es wachsen sehen möchten, unterstützen Sie uns!"
    Es -> "Si le gusta este proyecto y quiere verlo crecer, ¡apóyenos!"
    It -> "Se vi piace questo progetto e volete vederlo crescere, sosteneteci!"
    Ru ->
      "Если вам нравится этот проект и вы хотите видеть его развитие, поддержите нас!"
    Ko -> "이 프로젝트가 마음에 들고 성장하는 모습을 보고 싶다면 후원해 주세요!"
    Ja -> "このプロジェクトを気に入り、発展を望まれるなら、ぜひ応援してください！"
  }
  |> html.text
}

pub fn donate_bitcoin_address(l: Lang) -> element.Element(a) {
  case l {
    En -> "Bitcoin address: "
    Fr -> "Adresse bitcoin : "
    De -> "Bitcoin-Adresse: "
    Es -> "Dirección bitcoin: "
    It -> "Indirizzo bitcoin: "
    Ru -> "Биткойн-адрес: "
    Ko -> "비트코인 주소: "
    Ja -> "ビットコインアドレス: "
  }
  |> html.text
}

pub fn gallery_browse(l: Lang) -> String {
  case l {
    En -> "Browsing"
    Fr -> "Navigation"
    De -> "Durchstöbern"
    Es -> "Exploración"
    It -> "Esplorazione"
    Ru -> "Просмотр"
    Ko -> "탐색"
    Ja -> "閲覧"
  }
}

pub fn gallery_browse_comment(l: Lang) -> String {
  case l {
    En -> "Browsing files and folders of a peer"
    Fr -> "Navigation dans les fichiers et dossiers d'un pair"
    De -> "Durchstöbern der Dateien und Ordner eines Peers"
    Es -> "Exploración de los archivos y carpetas de un par"
    It -> "Esplorazione dei file e delle cartelle di un peer"
    Ru -> "Просмотр файлов и папок пира"
    Ko -> "피어의 파일과 폴더 탐색"
    Ja -> "ピアのファイルとフォルダーの閲覧"
  }
}

pub fn gallery_search(l: Lang) -> String {
  case l {
    En -> "Search result"
    Fr -> "Résultat de la recherche"
    De -> "Suchergebnis"
    Es -> "Resultado de la búsqueda"
    It -> "Risultato della ricerca"
    Ru -> "Результаты поиска"
    Ko -> "검색 결과"
    Ja -> "検索結果"
  }
}

pub fn gallery_search_comment(l: Lang) -> String {
  case l {
    En -> "The results are sorted by relevance. Folders are put on top."
    Fr ->
      "Les résultats sont triés par pertinence. Les dossiers sont placés en premier."
    De -> "Die Ergebnisse sind nach Relevanz sortiert. Ordner stehen oben."
    Es ->
      "Los resultados están ordenados por relevancia. Las carpetas se muestran arriba."
    It ->
      "I risultati sono ordinati per rilevanza. Le cartelle sono mostrate in alto."
    Ru ->
      "Результаты отсортированы по релевантности. Папки отображаются сверху."
    Ko -> "결과는 관련도 순으로 정렬되며, 폴더가 위에 표시됩니다."
    Ja -> "結果は関連度順に並べられ、フォルダーが上に表示されます。"
  }
}

pub fn gallery_download_folders(l: Lang) -> String {
  case l {
    En -> "Downloads - Folders"
    Fr -> "Transferts - Dossier"
    De -> "Downloads - Ordner"
    Es -> "Descargas - Carpetas"
    It -> "Download - Cartelle"
    Ru -> "Загрузки - Папки"
    Ko -> "다운로드 - 폴더"
    Ja -> "ダウンロード - フォルダー"
  }
}

pub fn gallery_download_folders_comment(l: Lang) -> String {
  case l {
    En ->
      "This view shows the files with their folders, they are both sorted alphabetically."
    Fr ->
      "Cette vue montre les fichiers avec leurs dossiers, ils sont triés alphabétiquement."
    De ->
      "Diese Ansicht zeigt die Dateien mit ihren Ordnern, beide alphabetisch sortiert."
    Es ->
      "Esta vista muestra los archivos con sus carpetas, ambos ordenados alfabéticamente."
    It ->
      "Questa vista mostra i file con le loro cartelle, entrambi in ordine alfabetico."
    Ru ->
      "В этом представлении файлы показаны вместе с папками, и те и другие отсортированы по алфавиту."
    Ko -> "이 화면은 파일을 폴더와 함께 보여 주며, 모두 이름순으로 정렬됩니다."
    Ja -> "このビューはファイルをフォルダーとともに表示します。どちらもアルファベット順に並びます。"
  }
}

pub fn gallery_download_files(l: Lang) -> String {
  case l {
    En -> "Downloads - Files"
    Fr -> "Transferts - Fichiers"
    De -> "Downloads - Dateien"
    Es -> "Descargas - Archivos"
    It -> "Download - File"
    Ru -> "Загрузки - Файлы"
    Ko -> "다운로드 - 파일"
    Ja -> "ダウンロード - ファイル"
  }
}

pub fn gallery_download_files_comment(l: Lang) -> String {
  case l {
    En ->
      "This view shows only the files, they can be rearranged, the top files are downloaded first."
    Fr ->
      "Cette vue montre seulement les fichiers, ils peuvent être réordonnés, les fichiers en haut sont téléchargés en premiers."
    De ->
      "Diese Ansicht zeigt nur die Dateien; sie können umsortiert werden, die obersten Dateien werden zuerst heruntergeladen."
    Es ->
      "Esta vista muestra solo los archivos; pueden reordenarse, los archivos de arriba se descargan primero."
    It ->
      "Questa vista mostra solo i file; possono essere riordinati, i file in alto vengono scaricati per primi."
    Ru ->
      "В этом представлении показаны только файлы; их порядок можно менять, верхние файлы загружаются первыми."
    Ko -> "이 화면은 파일만 보여 주며, 순서를 변경할 수 있고 위쪽 파일이 먼저 다운로드됩니다."
    Ja -> "このビューはファイルのみを表示します。並べ替えができ、上にあるファイルから先にダウンロードされます。"
  }
}

pub fn gallery_upload(l: Lang) -> String {
  case l {
    En -> "Upload view"
    Fr -> "Vue des envois"
    De -> "Upload-Ansicht"
    Es -> "Vista de subidas"
    It -> "Vista degli upload"
    Ru -> "Представление отдач"
    Ko -> "업로드 화면"
    Ja -> "アップロード画面"
  }
}

pub fn gallery_skin(l: Lang) -> String {
  case l {
    En -> "Skin"
    Fr -> "Skin"
    De -> "Skin"
    Es -> "Skin"
    It -> "Skin"
    Ru -> "Скин"
    Ko -> "스킨"
    Ja -> "スキン"
  }
}

pub fn download_button_download(l: Lang) -> element.Element(a) {
  case l {
    En -> "Download D-LAN"
    Fr -> "Télécharger D-LAN"
    De -> "D-LAN herunterladen"
    Es -> "Descargar D-LAN"
    It -> "Scarica D-LAN"
    Ru -> "Скачать D-LAN"
    Ko -> "D-LAN 다운로드"
    Ja -> "D-LAN をダウンロード"
  }
  |> html.text
}

pub fn download_button_version(
  l: Lang,
  version: String,
  platform: String,
) -> element.Element(a) {
  case l {
    En -> "Version " <> version <> " for " <> platform
    Fr -> "Version " <> version <> " pour " <> platform
    De -> "Version " <> version <> " für " <> platform
    Es -> "Versión " <> version <> " para " <> platform
    It -> "Versione " <> version <> " per " <> platform
    Ru -> "Версия " <> version <> " для " <> platform
    Ko -> "버전 " <> version <> " (" <> platform <> "용)"
    Ja -> "バージョン " <> version <> " (" <> platform <> " 用)"
  }
  |> html.text
}

pub fn download_button_released(l: Lang, date: String) -> element.Element(a) {
  case l {
    En -> "Released on " <> date
    Fr -> "Sorti le " <> date
    De -> "Veröffentlicht am " <> date
    Es -> "Publicado el " <> date
    It -> "Pubblicato il " <> date
    Ru -> "Дата выпуска: " <> date
    Ko -> "출시일 : " <> date
    Ja -> "リリース日: " <> date
  }
  |> html.text
}

pub fn download_button_torrent(l: Lang) -> element.Element(a) {
  case l {
    En -> "Download with BitTorrent"
    Fr -> "Télécharger avec BitTorrent"
    De -> "Mit BitTorrent herunterladen"
    Es -> "Descargar con BitTorrent"
    It -> "Scarica con BitTorrent"
    Ru -> "Скачать через BitTorrent"
    Ko -> "BitTorrent로 다운로드"
    Ja -> "BitTorrent でダウンロード"
  }
  |> html.text
}

pub fn current_lang(req: wisp.Request) -> Lang {
  // 1) Looks if a GET variable 'lang' is defined.
  use <- result.lazy_unwrap(
    req |> wisp.get_query |> list.key_find("lang") |> result.map(parse_lang),
  )
  // 2) Looks if a 'lang' value exist in a cookie.
  use <- result.lazy_unwrap(
    req |> wisp.get_cookie("lang", wisp.PlainText) |> result.map(parse_lang),
  )
  // 3) Looks in the "Accept-Language" HTTP header field.
  use <- result.lazy_unwrap(
    req
    |> accepted_langs_by_user_agent
    |> list.first
    |> result.map(parse_lang),
  )
  En
}

// Return a list of accepted languages by the user agent, sorted by quality,
// best first. Return only known languages.
// Read the HTTP field 'Accept-Language'.
fn accepted_langs_by_user_agent(req: wisp.Request) -> List(String) {
  let known_langs = all_langs() |> list.map(to_str)

  request.get_header(req, "accept-language")
  |> result.unwrap("")
  |> string.split(",")
  |> list.filter_map(fn(value) {
    // Extract the language and its quality: "fr-CH;q=0.8" -> #("fr-CH", 0.8).
    let #(lang_with_subtag, quality) = case
      string.split(string.trim(value), ";")
    {
      [lang, "q=" <> q] -> #(lang, parse_quality(q))
      [lang, ..] -> #(lang, 1.0)
      [] -> #("", 0.0)
    }
    // We don't care about the subtags: "fr-CH" -> "fr".
    let lang_str =
      lang_with_subtag |> string.split("-") |> list.first |> result.unwrap("")
    // We keep only known languages.
    case list.contains(known_langs, lang_str) {
      True -> Ok(#(lang_str, quality))
      False -> Error(Nil)
    }
  })
  // Sort by quality, bigger first.
  |> list.sort(fn(a, b) { float.compare(b.1, a.1) })
  // Remove the quality information.
  |> list.map(fn(lang_quality) { lang_quality.0 })
}

fn parse_quality(q: String) -> Float {
  case float.parse(q) {
    Ok(quality) -> quality
    // A quality like "q=1" isn't a valid float, try to parse it as an integer.
    Error(Nil) ->
      q |> int.parse |> result.map(int.to_float) |> result.unwrap(1.0)
  }
}
