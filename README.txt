% Progetto Prolog e Common Lisp 2018-01-15 (E1P)
% - json_parsing.pl -
%

La libreria sfrutta le dcg per la definizione della struttura degli oggetti da parsare.

La json_write si appoggia sul medesimo principio per mezzo della write_json_parse, che analizza
ricorsivamente le strutture passate nello stream in input e costruisce la stringa in sintassi JSON
standard, che viene infine passata alla write. 

Entrambe le funzioni implementano dei controlli sull'eventuale
esistenza del file (definito tramite Filename), tanto in lettura quanto in scrittura. 
L'unico problema riscontrato in tali controlli è l'impossibilità di eseguire la delete del file
dal sistema nel caso questo fosse presente al momento dell'invocazione della json_write 
(operazione che sembra richiedere diritti più elevati a livello di sistema). 
Questo controllo è 
stato inserito poiché l'opzione supersede inizializza correttamente la posizione per
la scrittura ad inizio file ma non rimuove l'eventuale contenuto già presente in esso, creando
problemi nel passaggio di oggetti dalla lunghezza inferiore rispetto al contenuto del file stesso,
limitandosi a sovrascrivere i caratteri necessari.

Per assicurare la compatibilità delle operazioni sequenziali di parse-write-load è stato quindi scelto
questo tipo di approccio.
 