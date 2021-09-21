# Syllabus #

## Einführung ##

Dieses Seminar führt Studierende systematisch an der Prozess der Reproduktion und Replikation von bereits publizierten Arbeiten heran. Zentraler Gedanke dieses Kurses ist es, dass Studierende durch das Replizieren von quantifizierenden Analysen anderer Wissenschaftler:innen besser mit fortgeschrittenen Methoden der sozialwissenschaftlichen Datenanalyse vertraut gemacht werden. Darüberhinaus befähigt das Seminar Studierende dazu angemessene empirische Modelle auszuwählen und erhört die Chance, dass Studierende bereits in einer frühen Karierrephase publizieren. 

Dieses Seminar ermöglicht es Studierende daher ein tiefgehenderes Verständnis von statistischen Modellen praxisbezogen zu erlangen. Ferner kann es Studierenden gelingen eine Replikationsstudie in einem Journal einzureichen, wenn durch die Replikationsstudie ein eigener substanzieller Beitrag zu Forschung geleistet wird (value added). 

## Dauer und Format ##

Dieser Kurs ist zweidimensional gestaltet. Erstens kann dieses Seminar als klassiche Veranstaltung im Rahmen eines Semesters abgehalten werden. Zweitens können die Ressourcen und Materialen auch als Selbstlernkurs für bereits fortgeschrittenen Studierende dienen, sich mit Replikationsstudien vertraut zu machen. 

Im klassischen Format eines Seminars während eines Semesters ist der Kurs auf zwölf Sitzungen ausgelegt. In den meisten dieser Sitzungen folgt einem kurzen Input der Seminarleitung ein extensiver praktischer Part, in denen Studierenden mit Hilfe statistischer Software (R oder STATA) an kleinen wöchentliche Aufgaben arbeiten und syetematisch angeleitet werden, die von Ihnen ausgewählte Studie zu reproduzieren bzw. zu replizieren. Studierende haben ferner regelhaft die Gelegenheit über ihre Probleme zu berichten und gemeinsam in den Austausch zu gehen. 

## Leistungsüberprüfung ##

Der Hauptfokus dieses Seminars ist das finale Projekt der Studierenden, in der Regel die Reproduktion und ggf. Ausweitung der Ergebnisse bereits publizierten Arbeiten (Replikation). Studierende fassen ihre Ergebnisse in einem Forschungsbericht zusammen und erläutern insbesondere was der ausgewählte Forschungsbereich von der Replikation lernt (value added). Ferner dokumentierten Sie alle Analyseschritte transparent und machen Sie entsprechende Softwareskripte und Daten Ihren Kommiliton:innen und der Seminarleitung zugänglich.

## Voraussetzungen ##

* Studierende beherrschen einfache statistische Analysen (deskriptive Statistik, einfache lineare Regressionsmodelle, t-test, F-test).
* Studierende können mit R oder STATA Datensätze einlesen und data wrangling sicher umsetzen (merging von Datensätzen, Filterung von Daten, Erstellen von Variablen).
* Studierende können pro Woche mindestens vier Zeitstunden für die Vor- und Nachbereitung des Kurses aufbringen.
* Ferner haben Studierende entweder R (inklusive RStudio) oder STATA als Software auf ihren Computern installiert und sind mit den jeweiligen Sprachen vertraut. 

## Vorbereitung auf die erste Sitzung ##

Machen Sie sich bereits Gedanken, in welchen Forschungsbereich Sie Ihre Replikationsstudie anfertigen möchten und suchen Sie nach geeigneten bereits publizierten Papern. Eine kurzer Leitfaden:
* Wählen Sie Paper nur dann aus, wenn die Replikationsmaterialen (Daten, Skripte) über die Zeitschriftenwebsite oder Dataverse, Github etc. öffentlich verfügbar sind. Das sollte aber mittlerweile Standard sein, ist es aber leider nicht immer. 
* Um die Wahrscheinlichkeit zu erhöhen, dass Ihre Replikationsstudie ggf. publiziert wird, wählen Sie Paper aus, welches in einem hochrangigen Journal veröffentlicht worden ist. 
* Das Paper sollte nicht älter als 2015 sein. 
* Das Paper sollte statistische Methoden verwenden, die Sie während der Seminarlaufzeit erlernen können!

## Ablaufplan ##

<table class="tg">
<tbody>
<tr>
<td width="20%"><strong>Woche 1</strong></td>
  <td width="80%"><p><h3>Einführungssitzung </h3></p> <p> Was ist Replikation? Was ist die “reproducibility debate” in der (statistischen) politikwissenschaftlichen Forschung; Warum sollten wir replizieren? Wie wählen Sie ein passendes Paper für eine Replikation aus? Wo finden Sie die Replikationsmaterialen? Wie kommunzieren Sie ggf. mit den Originalautoren? </p> <p><h3>Literatur</h3></p> 
  <p>King, Gary. 1995. Replication, Replication. PS: Political Science and Politics 28: 443–499. http://j.mp/jCyfF1</p>
  <p>King, Gary “How to Write a Publishable Paper as a Class Project ”, http://gking.harvard.edu/papers</p>
  <p>Nicole Janz, Bringing the Gold Standard into the Classroom: Replication in University Teaching, <em>International Studies Perspectives</em>, Volume 17, Issue 4, November 2016, Pages 392–407, https://doi.org/10.1111/insp.12104</p>
  </td>
</tr>

<tr>
<td width="20%"><strong>Woche 2</strong></td>
  <td width="80%"><p><h3> Was ist replizierbare sozialwissenschaftliche Datenanalyse? </h3></p> 
  <p> Was ist gute quantitative Forschung? Wie empirische Analysen anderer Forschender nachvollziehen und replizieren? Was verstehen wir unter Replikation, (Stichworte: Reproducibility vs. Replication; duplication study; replication study). Wie planen wir unsere Replikationsstudie? Wie arbeiten wir selber reproduzierbar? </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> Reproducible Research and Data Analysis. Open Science Training Handbook. https://open-science-training-handbook.gitbook.io/book/open-science-basics/reproducible-research-and-data-analysis </p>
  <p> Peng, Roger D. "Reproducible research in computational science." Science 334.6060 (2011): 1226-1227. https://www.science.org/doi/full/10.1126/science.1213847 </p>
  <p> Gandrud, Christopher. Reproducible research with R and RStudio. Chapman and Hall/CRC, 2018. https://api.taylorfrancis.com/content/books/mono/download?identifierName=doi&identifierValue=10.1201/9781315382548&type=googlepdf (zur Inspiration und zum Nachschlagen)</p>
  </td>
</tr>


<tr>
<td width="20%"><strong>Woche 3</strong></td>
  <td width="80%"><p><h3> Auswahl einer geeigneten Studie zur Replikation </h3></p> 
  <p> Suchen nach geeigneten quantifizierenden und bereits publizierten Studien; Daten und Softwareskripte zugänglich?; Verwendeten Methoden bekannt oder während des Seminars erlernbar?; Code und Daten herunterladen und übersichtliche Ordnerstruktur erstellen; Erste Replikationsanalyse: Kommt das raus, was im Paper steht?; Läuft der Code?</p> 
  
  <p><h3>Literatur</h3></p> 
  <p> keine Literatur, orientieren Sie sich an Gandrud, Christopher. Reproducible research with R and RStudio. Chapman and Hall/CRC, 2018</p>
  
  </td>
</tr>

<tr>
<td width="20%"><strong>Woche 4</strong></td>
  <td width="80%"><p><h3> Praktische Session </h3></p> 
  <p> Diskutieren erster Replikationsergebnisse; Wo tauchen Problem auf?; praktische Hilfe untereinander und durch Seminarleitung; Problemlösung und Verbessern der Skripte; Wie Ergebnisse speichern und Tabellen und Abbildungen reproduzierbar in Paper einbauen (Latex; Word; RMarkdown)</p> 
  
  <p><h3>Literatur</h3></p> 
  <p> keine Literatur</p>
  
  </td>
</tr>

<tr>
<td width="20%"><strong>Woche 5</strong></td>
  <td width="80%"><p><h3> Präsentation der Ergebnisse und Cross-Check </h3></p> 
  <p> Präsentieren der eigenen Reproduktivitätsanalysen der Originalstudie; Wo bestanden Probleme; Diskussion von Richtlinien des cross-checkings unter peers; Wie konstruktives Feedback geben?; Schreiben des Cross-Check Reports an peers </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> keine Literatur</p>
  
  </td>
</tr>

<tr>
<td width="20%"><strong>Woche 6</strong></td>
  <td width="80%"><p><h3> Cross-Check (keine Seminarsitzung)</h3></p> 
  <p> Durchführen des Cross-Checks für jeweiligen peer; Verbesserungen des eigenen Codes; Lesen mindestens eines veröffentlichten Replikationspapers: Wie ist die Gliederung aufgebaut; Wie trägt das Paper was zur Literatur bei (value added) </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> Herndon, Thomas, Michael Ash, and Robert Pollin. "Does high public debt consistently stifle economic growth? A critique of Reinhart and Rogoff." Cambridge Journal of Economics 38.2 (2014): 257-279. </p>
    <p> Bell, Mark S., and Nicholas L. Miller. "Questioning the effect of nuclear weapons on conflict." Journal of Conflict Resolution 59.1 (2015): 74-92. </p>
    <p> Gertler, Paul, Sebastian Galiani, and Mauricio Romero. "How to make replication the norm." Nature (2018): 417-419.</p>
    <p> Berinsky, A. J., Druckman, J. N., & Yamamoto, T. (2021). Publication Biases in Replication Studies. Political Analysis, 29(3), 370–384. </p>
    </td>
</tr>

<tr>
<td width="20%"><strong>Woche 7</strong></td>
  <td width="80%"><p><h3> Cross-Check Reports und weiterer Plan </h3></p> 
  <p> Vorstellen und Diskutieren der Cross-Check Reports; Problemlösung; Wie bringen wir eine Replikation in eine publikationswürdige Form?; Wie einen eigenen Forschungsbeitrag durch Replikation leisten? (Bsp. robustness checks; dummy variables; interactions; omitted variables; model selection, patterns of missings)  </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> keine Literatur</p>
    </td>
</tr>

<tr>
<td width="20%"><strong>Woche 8</strong></td>
  <td width="80%"><p><h3> Reproduktionsstudie zur Replikation mit Mehrwert machen </h3></p> 
  <p> Planen der weiteren Analyseschritte: 
  * 1) Originalergebnisse waren replizierbar? -> Wie Mehrwert bieten, der über die Replikationm hinausgeht?; 
  * 2) Originalergebnisse waren nicht replizierbar (bestätigt durch Seminarleitung) -> Warum waren diese nicht replizierbar? Wie Mehrwert bieten und fairen Report schreiben? </p> 
  
 <p> Im Fall 1): Mehrwert kann erst generiert werden, wenn Sie die substanzielle, problemspezifische Literatur kennen: Lesen und Sortieren. Im zweiten Schritt mit der   kleinstmöglichen Zahl an Verbesserungen starten, die neue und substanzielle Ergebnisse produziert: “missing data, selection bias, omitted variable bias, the model specification, differential item functioning, the functional form, etc., adding control variables or better measures, extending the time series and conducting out-of-sample tests, applying a better statistical model” (King 2006:120). </p> 
 
  <p> Im Fall 2): Mehrwert kann erst generiert werden, wenn Sie die herausfinden, warum Sie die Ergebnisse nicht reproduzieren können. Woran liegt das? Daten fehlerhaft, Skripte fehlerhaft; Was kommt bei Ihrer Replikation raus? Ändert das die substanzielle Interpretationen der Originalautor:innen? </p> 
  
  <p><h3>Literatur</h3></p> 
   <p>King, Gary. 1995. Replication, Replication. PS: Political Science and Politics 28: 443–499. http://j.mp/jCyfF1</p>
   <p>King, Gary “How to Write a Publishable Paper as a Class Project ”, http://gking.harvard.edu/papers</p>
   <p>King, Gary. 2006. “Publication, Publication.” PS: Political Science and Politics, 39, Pp. 119–125.</p>
    </td>
</tr>

<tr>
<td width="20%"><strong>Woche 9 (keine Seminarsitzung) </strong></td>
  <td width="80%"><p><h3> Reproduktionsstudie zur Replikation mit Mehrwert machen II </h3></p> 
  <p> Individuelle Arbeit an Projekt. Ausgedehnte Sprechstunde während des Sitzungzeit bei Probleme. Praktische Hilfestellungen bei statistische Fragen </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> keine Literatur</p>
    </td>
</tr>

<tr>
<td width="20%"><strong>Woche 10</strong></td>
  <td width="80%"><p><h3> Replikationscodes veröffentlichen </h3></p> 
  <p> Bringen Sie in die Sitzung Ihren kompletten Replikationsordner mit. Laufen alle Codes durch? Sind die Benennungen der Files (Tabellen, Figures) eindeutig?, Erstellen Sie ein readme-Dokument für die Nachvollziehbarkeit Ihrer Ordnerstruktur. Legen Sie Ihre Replikationscode inklusive der Ordnerstruktur und der Daten auf einem der folgenden Plattformen ab: Harvard Dataverse (https://dataverse.harvard.edu/); Open Science Framework (https://osf.io/); Github (https://github.com/).  </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> Alvarez, R. M., & Heuberger, S. (2021). How (Not) to Reproduce: Practical Considerations to Improve Research Transparency in Political Science. PS: Political Science & Politics, 1–6. https://doi.org/10.1017/S1049096521001062
</p>
    </td>
</tr>

<tr>
<td width="20%"><strong>Woche 11</strong></td>
  <td width="80%"><p><h3> Schreiben des Replikationspaper-Drafts </h3></p> 
  <p> Entwickeln Sie alleine oder im Team eine Struktur und eine Argumenationslinie für Ihr Replikationspaper. Schauen Sie dafür ggf. in der Sitzung 6 gelesenen Replikationsstudien nach Beispielen für gelungene Argumentationslinien und Gliederungen. Verteilen Sie ggf. Arbeitsaufgaben untereinander.   </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> keine Literatur</p>
    </td>
</tr>

<tr>
<td width="20%"><strong>Woche 12</strong></td>
  <td width="80%"><p><h3> Schreiben des Replikationspaper-Drafts II und Abschluss </h3></p> 
  <p>Präsentieren Sie Ihre Gliederung und inwiefern Sie durch die Replikation einen Forschungsbeitrag leisten konnten (value added) (Kurzpräsentation); Organisieren Sie sich in Kleingruppen, um in der sich nun anschließenden Verschriftlichung Ihrer Forschungsergebnisse peer-feedbacks zu geben. Abgabe der Hausarbeit fristgerecht; Kommen Sie in die Sprechstunden um ggf. Publikationsmöglichkeiten zu besprechen! Keep working on your paper; Abschlussrunde  </p> 
  
  <p><h3>Literatur</h3></p> 
  <p> keine Literatur</p>
    </td>
</tr>



</tbody>
</table>

