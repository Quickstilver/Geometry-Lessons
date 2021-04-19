(* ::Package:: *)

(* :Title: Progetto Mathematica *)
(* :Context: Lezione su cilindro *)
(* :Authors: I.I. S.B. D.F. *)
(* :Summary: This is a demonstration to calculate the surfaces of a cylinder *)
(* :Copyright: INF 8028 2020 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 12 *)
(* :History: *)
(* :Keywords: demonstration, cylinder, solid *)
(* :Sources: ???? *)
(* :Discussion: ???? *) 
BeginPackage["LezioneCilindro`"]

Suptotale::usage="Suptot dati il raggio r e l'altezza h calcola la superficie totale del cilindro e la confronta con il tuo risultato dicendo se \[EGrave] corretta o meno, \[Pi] \[EGrave] escluso dai calcoli e rimane simbolico, raggio e altezza vengono troncate alla seconda cifra decimale"
Suplaterale::usage="Suplaterale  dati il raggio r e l'altezza h calcola la superficie laterale del cilindro e la confronta con il tuo risultato dicendo se \[EGrave] corretta o meno, \[Pi] \[EGrave] escluso dai calcoli e rimane simbolico, raggio e altezza vengono troncate alla seconda cifra decimale"
Supbase::usage="Supbase dato il raggio r calcola la superficie di base del cilindro e la confronta con il tuo risultato dicendo se \[EGrave] corretta o meno, \[Pi] \[EGrave] escluso dai calcoli e rimane simbolico, il raggio viene troncato alla seconda cifra decimale"
Apricilindro::usage="Apricilindro permette di srotolare un cilindro su un piano 2d per individuare il rettangolo che costituisce la superficie laterla e i due cerchi che costituiscono la superficie di base"

Begin["`Private`"]




{rt,rl,rb}={1,1,1};    (*Inizializzazione per il raggio dei 3 esercizi*)
{ht,hl}={1,1};          (*Inizializzazione per l'altezza di Suptotale e Suplaterale*)
{bt,bl,bb}={"\nInserisci i dati ...","\nInserisci i dati ...","\nInserisci i dati ..."};   (*Inizializzione della variabile che contiene i feedback per l'utente*)


(*Suptotale \[EGrave] una funzione che controlla se l\[CloseCurlyQuote]input dell\[CloseCurlyQuote]utente corrisponde al risultato esatto calcolato.
All\[CloseCurlyQuote]utente vengono richiesti 3 input: 
- raggio
- altezza
- risultato
La funzione restituisce una stringa in base alle seguenti casistiche: 
- risultato giusto: "corretto"
-risultato errato: "Il risultato \[EGrave] sbagliato! Ricontrolla..."
-dati mancanti: "Uno o pi\[UGrave] campi sono vuoti!"*)	

Suptotale:= Manipulate[    (*creazione di un interfaccia  per gestire i parametri *)
            DynamicModule[{},   (*incapsulamento delle variabili dinamiche *)
            Column[{               (*dispone in colonna le label (Etichette) con i rispettivi campi di testo e i bottoni per il controllo delle operazioni;*)
            Labeled["Raggio:   ",InputField[Dynamic[rt],Number,FieldHint->"Raggio",FieldSize->Medium],Right,Editable->False], (* Prende in input numero, (Number: input solo numerico), aggiorna dinamicamente la variabile raggio, pone un FieldHint cio\[EGrave] testo di default e un'etichetta Label non modificabile (editable\[Rule]false)  *)
            Labeled["Altezza:  ",InputField[Dynamic[ht],Number,FieldHint->"Altezza",FieldSize->Medium],Right,Editable->False],  (* Lo stesso del caso precedente ma per l'altezza*)
            Labeled["Stot:  \[Pi] \[Times]",InputField[Dynamic[result],Number,FieldHint->"Stot",FieldSize->Medium],Right,Editable->False],  (*lo stesso del caso precedente ma per il risultato*)
            
 (* il bottone aggiorna il feeback dell'utente in seguito ad una valutazione del tipo IF*)
           Button["Calcola",bt=If[                                  (*la condizione IF verifica che lo scarto fra la superificie totale reale e quella inserita dall'utente sia compreso fra 0 e 0.5, vengono fatte operazioni di troncamento del tipo x100/100 per ridurre i calcoli a 2 cifre dopo la virgola*)
            0<= (Abs[(N[IntegerPart[Times[2N[Times[Abs[rt],100]/100] N[Times[Abs[ht],100]/100],100]]/100]+2*N[IntegerPart[ Times[ N[Times[Abs[rt],100]/100] ^2 ,100]]/100]) - N[IntegerPart[result 100]/100]])<0.5 
            ,"\nIl risultato \[EGrave] corretto!",  (*caso positivo dell'If*)
            "\nIl risultato \[EGrave] sbagliato! Ricontrolla...",  (*Caso  negativo dell'If*)
            "\nUno o pi\[UGrave] campi sono vuoti!"]],              (*Caso in cui un campo \[EGrave] NULL*)
           Button["Pulisci",{bt,result}={"\nInserisci i dati ...",0}],  (* Il bottone pulisci setta a 0 il risultato e cambia il feedback per l'utente*)
           Button["Genera esercizio",{rt,ht,result}={N[IntegerPart[RandomReal[{0,300},WorkingPrecision->5]100]/100],N[IntegerPart[RandomReal[{0,300},WorkingPrecision->5]100]/100],0}], (*Genera esercizio con valori reali random e pulisce il risultato*)
           Button["Ottieni risultato",bt="Il risultato corretto \[EGrave]: \[Pi]\[Times]" DecimalForm[N[IntegerPart[Times[2N[Times[Abs[rt],100]/100] N[Times[Abs[ht],100]/100],100]]/100]+2*N[IntegerPart[ Times[ N[Times[Abs[rt],100]/100] ^2 ,100]]/100],Infinity]],
           
           

Text[Style[bt, 14, FontFamily->"Menlo", FontColor->Black]]}]],   (* setting dei parametri stilistici*)
                    Dynamic[Graphics3D[{Cylinder[{{0,0,Abs[ht]},{0,0,Abs[ht]+.001}},Abs[rt]],Cylinder[{{0,0,0},{0,0,.001}},Abs[rt]],Opacity[.5],Cylinder[{{0,0,0},{0,0,Abs[ht]}},Abs[rt]]},SphericalRegion->True,Boxed->False,ViewAngle->18 Degree]],
		ControlPlacement->Right] (*la dynamic intercetta i dati inseriti dall'utente e crea un cilindro con quelle dimensioni*)
		
		

(*Suplaterale \[EGrave] una funzione che controlla se l\[CloseCurlyQuote]input dell\[CloseCurlyQuote]utente corrisponde al risultato esatto calcolato.
All\[CloseCurlyQuote]utente vengono richiesti 3 input: 
- raggio
- altezza
- risultato
La funzione restituisce una stringa in base alle seguenti casistiche: 
- risultato giusto: "corretto"
-risultato errato: "Il risultato \[EGrave] sbagliato! Ricontrolla..."
-dati mancanti: "Uno o pi\[UGrave] campi sono vuoti!"*)		
		
Suplaterale:=Manipulate[ DynamicModule[{},
            Column[{
            Labeled["Raggio:   ",InputField[Dynamic[rl],Number,FieldHint->"Raggio",FieldSize->Medium],Right,Editable->False], 
            Labeled["Altezza:  ",InputField[Dynamic[hl],Number,FieldHint->"Altezza",FieldSize->Medium],Right,Editable->False],
            Labeled["SLat:  \[Pi] \[Times]",InputField[Dynamic[resultl],Number,FieldHint->"SLat",FieldSize->Medium],Right,Editable->False], 
            (*superficie laterale calcolata come (2rh),viene verificato che lo scarto fra la superficie reale e il risultato dell'utente sia compreso fra 0 e 0.5*)
Button["Calcola",bl=If[0<= (Abs[N[IntegerPart[Times[2N[Times[Abs[rl],100]/100] N[Times[Abs[hl],100]/100],100]]/100] - N[IntegerPart[resultl 100]/100]])<=0.5 ,"\nIl risultato \[EGrave] corretto!","\nIl risultato \[EGrave] sbagliato! Ricontrolla...", "\nUno o pi\[UGrave] campi sono vuoti!"]],
Button["Pulisci",{bl,resultl}={"\nInserisci i dati ...",0}],
 Button["Genera esercizio",{rl,hl,resultl}={N[IntegerPart[RandomReal[{0,300},WorkingPrecision->5]100]/100],N[IntegerPart[RandomReal[{0,300},WorkingPrecision->5]100]/100],0}],
 Button["Ottieni risultato",bl="Il risultato corretto \[EGrave]: \[Pi]\[Times]" DecimalForm[N[IntegerPart[Times[2N[Times[Abs[rl],100]/100] N[Times[Abs[hl],100]/100],100]]/100],Infinity]],
Text[Style[bl, 14, FontFamily->"Menlo", FontColor->Black]]}]],
                    Dynamic[Graphics3D[{Cylinder[{{0,0,Abs[hl]},{0,0,Abs[hl]+.001}},Abs[rl]],Cylinder[{{0,0,0},{0,0,.001}},Abs[rl]],Opacity[.5],Cylinder[{{0,0,0},{0,0,Abs[hl]}},Abs[rl]]},SphericalRegion->True,Boxed->False,ViewAngle->18 Degree]],
		ControlPlacement->Right]
								

(*Supbase \[EGrave] una funzione che controlla se l\[CloseCurlyQuote]input dell\[CloseCurlyQuote]utente corrisponde al risultato esatto calcolato.
All\[CloseCurlyQuote]utente vengono richiesti 3 input: 
- raggio
- risultato
La funzione restituisce una stringa in base alle seguenti casistiche: 
- risultato giusto: "corretto"
-risultato errato: "Il risultato \[EGrave] sbagliato! Ricontrolla..."
-dati mancanti: "Uno o pi\[UGrave] campi sono vuoti!"*)									
																								
Supbase := Manipulate[ DynamicModule[{},
            Column[{
            Labeled["Raggio:    ",InputField[Dynamic[rb],Number,FieldHint->"Raggio",FieldSize->Medium],Right,Editable->False],
            
            Labeled["Sbase:  \[Pi] \[Times]",InputField[Dynamic[resultb],Number,FieldHint->"Sbase",FieldSize->Medium],Right,Editable->False],
            (* superficie di base calcolata come r^2, viene verificato che lo scarto fra la superficie reale e il risultato dell'utente sia compreso fra 0 e 0.5*)
Button["Calcola",bb=If[0<=(Abs[N[IntegerPart[ Times[ N[Times[Abs[rb],100]/100] ^2 ,100]]/100] - N[IntegerPart[resultb 100]/100]])<= 0.5 ,"\nIl risultato \[EGrave] corretto!","\nIl risultato \[EGrave] sbagliato! Ricontrolla...", "\nUno o pi\[UGrave] campi sono vuoti!"]],
Button["Pulisci",{bb,resultb}={"\nInserisci i dati ...",0}],
 Button["Genera esercizio",{rb,resultb}={N[IntegerPart[RandomReal[{0,300},WorkingPrecision->5]100]/100],0}],
Button["Ottieni risultato",bb="Il risultato corretto \[EGrave]: \[Pi]\[Times]" DecimalForm[ N[IntegerPart[ Times[ N[Times[Abs[rb],100]/100] ^2 ,100]]/100] ,Infinity]],
Text[Style[bb, 14, FontFamily->"Menlo", FontColor->Black]]}]],
                    Dynamic[Graphics3D[{Cylinder[{{0,0,Abs[rb]},{0,0,Abs[rb]+.001}},Abs[rb]],Cylinder[{{0,0,0},{0,0,.001}},Abs[rb]],Opacity[.5],Cylinder[{{0,0,0},{0,0,Abs[rb]}},Abs[rb]]},SphericalRegion->True,Boxed->False,ViewAngle->18 Degree]],
		ControlPlacement->Right]
		
(* Questa funzione permette all'utente di manipolare un cilindro a partire dalla sua forma tridimensionale fino al suo sviluppo bidimensionale
L'utene ha a disposizione un vasto grado di libert\[AGrave] che gli permette la manipolazione dei seguenti elementi rappresentati da uno slider:
- "Raggio" definisce un range di valori (0,1) per il raggio delle due basi;
  - "Altezza" definisce il range di valori (0,1) per l'altezza del cilindro, nonch\[EGrave] per il lato minore del rettangolo;
  - "Ruota" che permette l'apertura delle basi del cilindro;
  - "Srotola" permette lo srotolamento della superficie laterale nel suo rispettivo rettangolo.
*)
Apricilindro:= Manipulate[
(* "da" corrisponde a 6\[Degree] ed \[EGrave] l'intervallo discreto con cui avviene la rotazione dei poligoni *)
With[{da=2Pi/60},
With[{
(* "r1" e "r2" rappresentano le funzioni di trasformazione dei due cerchi che compongono le basi del cilindro.
Il valore s1=[0,1] viene moltiplicato per Pi/2 (90\[Degree]) permettendo l'apertura di 0-90\[Degree] 
Il secondo e terzo input definiscono il limite minimo e massimo per la posizione sull'asse x,y,z *)
r1=RotationTransform[s1*Pi/2,{1,0,0},{0,h/2,0}],
r2=RotationTransform[-s1*Pi/2,{1,0,0},{0,-h/2,0}]},
With[{
(* Definizione lista di valori per costruzione dinamica del primo cerchio *)
d1=Table[
r1[{
r Cos[a],                          (* coordinate x al variare del raggio "r" e angolo "a" *)
r Sin[a]+h/2+r,      (* coordinate y al variare dell'altezza "h" e raggio "r" *)
0}],                                    (* asse z che rimane invariato *)
{a,0,2Pi,da}],            (* angolo a \[Rule] valore compreso [0, 2Pi] con intervallo da (6\[Degree]). *)
(* Definizione lista di valori per costruzione dinamica del secondo cerchio *)
d2=Table[
r2[{
r Cos[a],
r Sin[a]-h/2-r, 
0}],
{a,0,2Pi,da}]},
(* Costruzione del cilindro 3D a partire dalle sue figure primitive *)
Graphics3D[{Opacity[.99],EdgeForm[],
Polygon[d1], (* Cerchio prima base con valore di "d1" *)
Polygon[d2], (* Cerchio seconda base con valori di "d2" *)
(* Costruzione superficie laterale (rettangolo dinamico) *)
Table[
(* Estensione dell'rettangolo nello sviluppo (rettangolo srotolato)*)
Polygon[{
(* Definizione punti che formano il rettangolo sull'asse x,y,z *)
{-a,+h/2,0},
{-a+r da,h/2,0},
{-a+r da,-h/2,0},
{-a,-h/2,0}}],
  (* "a" indica la posizione del punto al variare del raggio "r" e angolo "da".
Al convergere dello slider "s2" (srotola) al valore 0, lo sviluppo della superficie laterale incrementa *)
{a,0,+(1-s2)(r 2Pi)-r da,r da}],
Take[
Table[
(* Rettangolo arrotolato *)
Polygon[{
(* Definizione punti che formano la superficie laterale arrotolata sull'asse x,y,z *)
{r Cos[a],+h/2,r+r Sin[a]},
{r Cos[a+da],h/2,r+r Sin[a+da]},
{r Cos[a+da],-h/2,r+r Sin[a+da]},
{r Cos[a],-h/2,r+r Sin[a]}}],
(* "a" varia da -90\[Degree] a +270\[Degree] *)
{a,-Pi/2,-Pi/2+(2Pi-2Pi/60),2Pi/60}],
Round[s2 60]]},
ImageSize->{500,400},   (* dimensione finestra figura *)
Boxed->False,                      (* rimozione box attorno graphics3D *)
SphericalRegion->True,  (* permette di evitare distorsioni durante il movimento *)
PlotRange->{Automatic,{-3,3},{0,2}}
]]]],
{{r,1/2,"raggio"},.1,1}, (* Slider raggio *)
{{h,1,"altezza"},.1,1},    (* Slider altezza *)
{{s1,1,"ruota"},0,1},         (* Slider rotazione basi *)
{{s2,0,"srotola"},0,1}        (* Slider srotolamento superficie laterale *)
]
  
								
End[]
EndPackage[]
