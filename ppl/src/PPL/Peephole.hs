<disableusecache docref="Instructions.hs">
<disableusecache docref="Assemble.hs">
<disableusecache docref="stmt.ppl">
<disableusecache docref="stmt.gencode">
<disableusecache docref="stmt.ass">

<style style="comp.style">

<stdheader>

<descrpar>Assemblieren<descrpar1>

<descr>Zielmaschine<descr1>
Die Zielmaschine
soll mit einem Programmzaehler fuer die
Ablaufsteuerung arbeiten. Das bedeuted, dass die symbolischen
Marken fuer Sprungziele in den Instrunktionen ersetzt werden muessen
durch Codepositionen.
<markinpage>

<anddescr>Sprung- Instruktionen<descr1>
In dieser Maschine sollen keine absoluten Codepositionen
in den Instruktionen stehen, sondern Distanzen ausgehend
von der Position der Sprung-Instruktion.
Der Vorteil dieses Ansatzes besteht darin, dass der
Code im Speicher verschoben werden kann, ohne die
Sprung-Instruktionen modifizieren zu muessen.
<markinpage>

<anddescr>Assemblieren<descr1>
Das Zusammensammeln (Assemblieren) besteht hier
also nur im Aufbau einer Tabelle fuer Sprungmarken
und in der Ersetzung der Marken in den Sprungbefehlen
durch Distanzen.
<markinpage>

</descr>
</descrpar>

<includehaskell
 text="Die Datenstruktur fuer die Maschineninstruktionen"
 ref="Instructions.hs"
>

<includeppl
 text="Anweisungen: die Quelle: stmt.ppl"
 ref="stmt.ppl"
>

<includepplass
 text="Anweisungen: der Assemblercode"
 ref="stmt.gencode"
>

<includepplass
 text="Anweisungen: der Maschinencode"
 ref="stmt.ass"
>

<includehaskell
 text="Das Assemblieren: Assemble.hs"
 ref="Assemble.hs"
>

</stdheader>