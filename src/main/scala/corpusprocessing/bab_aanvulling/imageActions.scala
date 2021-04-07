package corpusprocessing.bab_aanvulling
import corpusprocessing.bab_aanvulling.imageCorrections.{∀, _}

object imageActions {
 val left = 270
 val right = 90
 val actions = List(
  // 1154	1e foto (adres) 180 roteren; 2e foto (brief) links roteren
  "bab1154" -> List(rotate(0, 0, 180, true), rotate(1, 1, left, true)),
  // 1146	1e foto 180 roteren (adres)
  "bab1146" -> List(rotate(0, 0, 180, true)),
  // 1705	1e foto 180 roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1705" -> List(patch(0, length), rotate(0, 0, 180, true)),
  // 1426	1e foto 180 roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1426" -> List(patch(0, length), rotate(0, 0, 180, true)),
  // 1458	1e foto 180 roteren (adres); huidige 1e foto toevoegen als 5e foto
  "bab1458" -> List(patch(0, length, false), rotate(0, 0, 180, true)),
  // 1422	1e foto handhaven (adres), maar opnieuw toevoegen rechts geroteerd als 3e foto
  "bab1422" -> List(rotate(0, 2, right, false)),
  // 844	1e foto links roteren (adres)
  "bab844" -> List(rotate(0, 0, left, true)),
  // 845	1e foto links roteren (adres)
  "bab845" -> List(rotate(0, 0, left, true)),
  // 1153	1e foto links roteren (adres)
  "bab1153" -> List(rotate(0, 0, left, true)),
  // 1878	1e foto links roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1878" -> List(patch(0, 2, false), rotate(0, 0, left, true)),
  // 845	1e foto links roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab845" -> List(patch(0, 2, false), rotate(0, 0, left, true)),
  // 1835	1e foto links roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1835" -> List(patch(0, 2, false), rotate(0, 0, left, true)),
  // 1155	1e foto links roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1155" -> List(patch(0, 2, false), rotate(0, 0, left, true)),
  // 1182	1e foto links roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1182" -> List(patch(0, 2, false), rotate(0, 0, left, true)),
  // 2100	1e foto rechts roteren (adres)
  "bab2100" -> List(rotate(0, 0, right, true)), // RONDE2
  // 1506	1e foto rechts roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1506" -> List(patch(0, 2, false), rotate(0, 0, right, true)),
  // 1804	1e foto rechts roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab1804" -> List(patch(0, 2, false), rotate(0, 0, right, true)),
  // 2273	1e foto rechts roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab2273" -> List(patch(0, 2, false), rotate(0, 0, right, true)),
  // 1910	2e foto (brief) ontbreekt; dat moet de eerder aangeleverde foto 15-06-2009 163 zijn
  "bab1910" -> List(), // opgelost met text file aanpassing?
  // 1139	2e foto (brief) rechts roteren
  "bab1139" -> List(rotate(1, 1, right, true)),
  // 1728	2e foto (brief) rechts roteren
  "bab1728" -> List(rotate(1, 1, right, true)),
  // 1570	2e foto nog een keer opnemen als 3e foto nu links geroteerd (tekst in marge)
  "bab1570" -> List(rotate(1, 2, left, false)),
  // 1920	2e foto rechts roteren (brief)
  "bab1920" -> List(rotate(1, 1, right, true)),
  // 1477	2e foto rechts roteren (brief); huidige 2e foto handhaven als 3e foto
  "bab1477" -> List(), // List(patch(1, 2, false), rotate(1, 1, right, true)), RONDE2
  // 1478 2e foto rechts 90 roteren en vervolgens huidige 2e foto handhaven als 3e foto
  "bab1478" -> List(patch(1, 2, false), rotate(1, 1, right, true)), // RONDE2
  // 104	2e foto rechts roteren (brief); huidige 2e foto handhaven als 3e foto
  "bab104" -> List(patch(1, 2, false), rotate(1, 1, right, true)),
  // 1769	2e foto rechts roteren (brief); huidige 2e foto handhaven als 3e foto (tekst in marge)
  "bab1769" -> List(patch(1, 2, false), rotate(1, 1, right, true)),
  // 1820	3e foto rechts roteren
  "bab1820" -> List(rotate(2, 2, right, true)),
  // 1476	2e foto is onzichtbaar. Dat moet de niet geleverde 06-01-2010 522 zijn (gaat hierbij); die 2e foto moet ook nog rechts geroteerd worden (tekst in marge) 3e foto (gaat hierbij)
  // 	N.B.  De geleverde 06-01-2010 520 (en 06-01-2010 521) zijn niet correcte foto's van dezelfde brief
  "bab1476" -> List(rotate(1,2, left)), // , rotate(2,3,right)), // RONDE2
  // 2029	foto 3 moet foto 2 worden (en de oorsp foto 2 dan J); na  brief  moet foto 3 links geroteerd foto 4 worden ????
  "bab2029" -> List(swap(1,2), rotate(1, length, left, false)),
  // 759	2e foto herhalen links geroteerd (vanwege tekst marge) als foto 3
  "bab759" -> List(rotate(1, 2, left, false)),
  // 2049	2e foto (adres) links roteren
  "bab2049" -> List(rotate(1, 1, left, true)),
  // 1657	1e foto links roteren
  "bab1657" -> List(rotate(0, 0, left, true)),
  // 1521	2e foto rechts roteren; huidige 2e foto wordt 3e foto (tekst in marge)
  "bab1521" -> List(rotate(1,1,right)),
  // 1545	2e foto links geroteerd moet foto 3 worden (vanwege tekst in marge)
  "bab1545" -> List(rotate(1,2,left)),
  // 2121	1e foto links roteren (vanwege tekst in marge) wordt als foto 2 ingevoegd
  "bab2121" -> List(rotate(0,1,left)),
  // 2119	1e foto links roteren (vanwege tekst in marge) wordt als foto 2 ingevoegd
  "bab2119" -> List(rotate(0,1,left)),
  // 1514	1e foto links roteren (adres)
  "bab1514" -> List(rotate(0,0,left,true)),
  // 2170	1e foto links roteren (vanwege tekst in marge) wordt als foto 2 ingevoegd
  "bab2170" -> List(rotate(0,1,left)),
  // 991	1e foto 180 roteren (adres); huidige 1e foto toevoegen als 3e foto
  "bab991" -> List(patch(0,2), rotate(0,0,180,true)),
  // 1548	2e foto links roteren (vanweg tekst in marge) en zo invoegen als foto 3
  "bab1548" -> List(rotate(1,2,left)),
  // 1172	1e en 2e foto allebei links roteren
  "bab1172" -> List(rotate(0,0,left,true), rotate(1,1,left,true)),
  // 1291	2e foto rechts roteren (brief); huidige 2e foto handhaven als 3e foto (tekst in marge)
  "bab1291" -> List(patch(1,2,true), rotate(1,1,right,true)),
  // 2296	1e foto 180 roteren en toevoegen als 3e foto
  "bab2296" -> List(rotate(0,2,180)),
  // 86	1e foto 180 roteren (adres) en invoegen als foto 2; huidige 3e foto moet laatste foto worden;
  // 86	huidige 4e foto moet voorlaatste foto worden en ook moet die rechts geroteerd nogmaals na nieuwe foto 3 geplaatst worden
  "bab86" -> List(assign(List(0, rot(0,180),1, rot(3,right),3,2))), // List(swap(2,last), remove(2), rotate(0,1,180),  rotate(2,3,right)),
  // 1568	2e foto (adres) moet 1e foto worden; 3e foto moet nieuwe 2e foto worden en ook links geroteerd de nieuwe foto 3;
  // 1568	1e foto wordt nieuwe foto 4 en geroteerd nieuwe foto 5 ??? Welke hoek???
  "bab1568" -> List(assign(List(1, 2, rot(2,left), 0, rot(0,left)))),
  // 2373	1e foto rechts roteren (adres)
  "bab2373" -> List(rotate(0,0,right,true)),
  // 2378	3e foto links roteren en invoegen als foto 4
  "bab2378" -> List(rotate(2,3,left,false)),
  // 2377	1e foto links roteren en invoegen als foto2, huidige laatste foto rechts roteren en invoegen als voorlaatste foto
  "bab2377" -> List(rotate(0,1,left), rotate(last, length, right), swap(penultimate,last)),
  // 2363	2e foto rechts roteren en invoegen als nieuwe foto 2; huidige foto2 wordt foto3
  "bab2363" -> List(rotate(1,1,right)),
  // 2365	2e foto rechts roteren en invoegen als nieuwe foto 2; huidige foto2 wordt foto3
  "bab2365" -> List(rotate(1,1,right)),
  // 2366	laatste foto links roteren en invoegen als nieuwe laatste foto
  "bab2366" -> List(rotate(last,length,left), rotate(1,length,left), remove(1)), // RONDE2
  // 2364	1e foto rechts roteren (adres)
  "bab2364" -> List(rotate(0,0,right, true)),
  // 2367	laatste foto links roteren (vanwege tekst marge) en invoegen als nieuwe laatste foto
  "bab2367" -> List(rotate(last,length,left)),
  // 5	1e foto links roteren, 2e foto rechts roteren
  "bab5" -> List(rotate(0,0,left,true)),// rotate(1,1,right,true)), // !! tweede image niet roteren? RONDE2!
  // 1578	3e foto links roteren
  "bab1578" -> List(rotate(2,2,left,true)),
  // 1626	1e en 2e foto links roteren
  "bab1626" -> List(rotate(0,0,left,true), rotate(1,1,left,true)),
  // 1627	2e foto links roteren
  "bab1627" -> List(rotate(1,1,left,true)),
  // 1624	1e foto links roteren (adres)
  "bab1624" -> List(rotate(0,0,left,true)),
  // 1000	1e foto 180 roteren en als nieuwe foto 2 invoegen
  "bab1000" -> List(rotate(0,1,180,false)),
  // 2361	foto 2, 3 en 4 rechts roteren
  "bab2361" -> List(rotate(1,1,right,true), rotate(2,2,right,true), rotate(3,3,right,true)),
  // 1265	foto 2 moet laatste foto worden en daarna nog een keer 180 geroteerd toevoegen
  // 5e foto ontbreekt (tekst en Voorregte de Christene). Die foto 05-01-2010 168 moet ingevoegd worden voor de huidige 5e en 6e foto; voor de zekerheid lever ik hem nog een keer
  "bab1265" -> List(patch(1,length), remove(1), rotate(last,length,180)), // remove toegevoegd // RONDE2
  // 1512	foto 2 (met hand) verwijderen
  "bab1512" -> List(remove(1)),
  // 1502	alle foto's ontbreken, moeten de volgende zijn: 06-01-2010 541, 06-01-2010 543, 06-01-2010 544, 06-01-2010 545, 06-01-2010 545A. Hierbij aangeleverd, met de betreffende textfile
  "bab1502" -> List(), // assign(List("extra/06-01-2010 541", "extra/06-01-2010 543", "extra/06-01-2010 544", "extra/06-01-2010 545", "extra/06-01-2010 545A"))),
  // 1710	2e foto links roteren en invoegen als 3e foto
  "bab1710" -> List(rotate(1,2,left)),
  // 1632	3e foto links roteren
  "bab1632" -> List(rotate(2,2,left,true)),
  // 1616	2e en 3e foto links roteren
  "bab1616" -> List(rotate(1,1,left,true), rotate(2,2,left,true)),
  // 1617	2e en 3e foto links roteren; 4e foto ontbreekt, is de al eerder aangeleverde foto 07-01-2010 172 die ook links geroteerd moet worden
  "bab1617" -> List(∀(_.indices.drop(1), i => rotate(i,i,left,true))),
  // 79	alle foto's rechts roteren
  "bab79" -> List(), // List(rotAll(right)), // NEE, NIET RONDE2
  // 1622	2e foto links roteren (vanwege tekst marge) en als 3e foto invoegen; 1e foto herhalen als 4e foto
  "bab1622" -> List(rotate(1,2,left), patch(0,3)),
  // 1623	1e en 2e foto links roteren
  "bab1623" -> List(rotAll(left)),
  // 1629	alle foto's links roteren en laatste foto 180 geroteerd invoegen
  "bab1629" -> List(assign(List(rot(0,left), rot(1,left), rot(2,left), rot(2,180)))),
  // 1520	alle foto's links roteren
  "bab1520" -> List(rotAll(left)),
  // 1519	alle foto's links roteren
  "bab1519" -> List(rotAll(left)),
  // 2427	2e foto links roteren (tekst in marge) en als 3e foto invoegen
  "bab2427" -> List(rotate(1,2,left)),
  // 73	1e foto 180 roteren (adres) en als 2e foto invoegen; alle volgende foto's rechts roteren
  "bab73" -> List(rotate(0,1,180)), // ,rotate(2,2,right,true),rotate(3,3,right,true),rotate(4,4,right,true)), // RONDE2 geen rechterrotatie
  // 877	foto's zijn in verkeerde onduidelijke volgorde gefotografeerd; daarom nieuwe textfile KB 336-166-1-6.txt en nieuwe foto's aangeleverd in plaats van KB 336-166-171 en de daarmee corresponderende foto's
  "bab877" -> List(),
  // 1747	foto 3 links roteren en huidige foto 3 wordt foto 4
  "bab1747" -> List(patch(2,3),rotate(2,2,left,true)),
  // 2510	foto 5 ontbreekt. Dat moet de niet geleverde 2013_aug-0996 zijn (gaat hierbij; is eerder ook op 18 aug 2919 geleverd)
  "bab2510" -> List(),
  // 501	1e foto links roteren (adres); huidige 1e foto als 3e foto invoegen
  "bab501" -> List(patch(0,2),rotate(0,0,left,true)),
  // 502	1e foto links roteren (adres); huidige 1e foto als 3e foto invoegen
  "bab502" -> List(patch(0,2),rotate(0,0,left,true)),
  // 2192	1e foto 180 roteren (adres); alle volgende foto's links roteren
  "bab2192" -> List(rotate(0,0,180,true), rotate(1,1,left,true), rotate(2,2,left,true), rotate(3,3,left,true)),
  // 2315	1e foto 180 roteren (adres); alle volgende foto's links roteren
  "bab2315" -> List(rotate(0,0,180,true), rotate(1,1,left,true), rotate(2,2,left,true),
   rotate(3,3,left,true), rotate(4,4,left,true), rotate(5,5,left,true), rotate(6,6,left,true),
   rotate(7,7,left,true), rotate(8,8,left,true)),
  // 276	1e en 2e foto allebei links roteren
  "bab276" -> List(), // List(rotate(0,0,left,true), rotate(1,1,left,true)), //  NEE NIET RONDE2
  // 2501	1e foto verwijderen (foutief; 2013_aug-0884.jpg) en textfile 2013_aug-0884-0888.txt vervangen door nieuwe 2013_aug-0885-0888.txt (hierbij aangeleverd)
  "bab2501" -> List(),
  // 875	2e foto ontbreekt. Dat is de eerder geleverde KB 336-162. Fout komt omdat in de textfile die foto niet is opgenomen. Daarom lever ik nieuwe textfile KB 336-161-163 aan zodat die de juiste foto's koppelt.
  "bab875" -> List(),
  // 1411	laatste foto rechts roteren en achteraan invoegen (tekst in marge)
  "bab1411" -> List(rotate(last,length,right)),
  // 142	1e foto 180 roteren (adres); alle volgende foto's links roteren
  "bab142" -> List(rotate(0,0,180,true), rotate(1,1,left,true), rotate(2,2,left,true),
   rotate(3,3,left,true)),
  // 1759	laatste foto links roteren (vanwege tekst marge) en invoegen als nieuwe laatste foto
  "bab1759" -> List(rotate(last,length,left,false)),
  // 2254	1e foto 180 roteren (adres); alle volgende foto's links roteren
  "bab2254" -> List(rotate(0,0,180,true), ∀(s => (1 until s.size), i => rotate(i,i,left,true))),
  // 1383	1e foto links roteren en als 2e foto invoegen; laatste foto ook links roteren en als allerlaatste invoegen;
  "bab1383" -> List(rotate(0,1,left), rotate(last,length,left)),
  // 2330	1e foto 180 roteren (adres), 2e foto links roteren (tekst marge) en invoegen als 3e foto, laatste foto rechts roteren en invoegen als voorlaatste
  "bab2330" -> List(rotate(0,0,180,true), rotate(1,2,left,false), rotate(last, penultimate,right), swap(3,4)), // RONDE 2 swap toegevoegd
  // 2093	alle foto's links roteren, huidige 1e foto 180 roteren (tekst marge) en invoegen als 2e foto
  "bab2093" -> List(rotate(0,1,180),rotate(0,0,left,true), ∀(s => (2 until s.size), i => rotate(i,i,left,true))),
  // 2508	alle foto's links roteren behalve de 1e foto
  "bab2508" -> List(∀(s => (1 until s.size), i => rotate(i,i,left,true))),
  // 2509	3e foto (2013_aug-0985) deleren, alle foto's links roteren; nieuwe textfile 2013_aug-0983-0987 omdat in de huidige 0985 staat vermeld
  "bab2509" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 2174	laatste foto links roteren en invoegen als nieuwe laatste;
  "bab2174" -> List(rotate(last,length,left)),
  // 2522	alle foto's rechts roteren behalve 1e foto en foto 17
  "bab2522" -> List(∀(s => (1 until 16) ++ (17 until s.size), i => rotate(i,i,right,true))),
  // 75	alle foto's rechts roteren
  "bab75" -> List(), //  List(∀(s => s.indices, i => rotate(i,i,right,true))), // NEE, niet RONDE2
  // 1552	alle foto's rechts roteren
  "bab1552" -> List(∀(s => s.indices, i => rotate(i,i,left,true))), // moet links zijn?
  // 1633	alle foto's links roteren
  "bab1633" -> List(∀(s => s.indices, i => rotate(i,i,left,true))),
  // 1097	2e foto (brief) rechts roteren, huidige 2e foto wordt 3e foto
  "bab1097" -> List(patch(1,2), rotate(1,1,right,true)),
  // 1581	alle foto's links roteren
  "bab1581" -> List(∀(s => s.indices, i => rotate(i,i,left,true))),
  // 1628	alle foto's links roteren
  "bab1628" -> List(∀(s => s.indices, i => rotate(i,i,left,true))),
  // 2431	2e, 3e en 4e foto links roteren
  "bab2431" -> List(∀(s => Seq(1,2,3), i => rotate(i,i,left,true))),
  // 1386	3e foto links roteren (tekst in marge)en toevoegen als 4e foto
  "bab1386" -> List(rotate(2,3,left)),
  // 1937	1e foto 180 roteren (adres), huidige 1e foto wordt 2e foto
  "bab1937" -> List(patch(0,1), rotate(0,0,180,true)),
  // 2331	1e foto 180 roteren (adres), volgende foto's links roteren
  "bab2331" -> List(rotate(0,0,180,true), ∀(s => s.indices.drop(1), i => rotate(i,i,left,true))),
  // 2197	1e foto 180 roteren (adres), alle volgende foto's links roteren, 5e foto 180 geroteerd na huidige foto5 invoegen, 6e foto 180 geroteerd na huidige foto6
  "bab2197" -> List(rotate(0,0,180,true), rotate(5,6,180), rotate(4,5,180), ∀(s => s.indices.drop(1).diff(List(5,7)), i => rotate(i,i,left,true))),
  // 2267	1e foto 180 roteren (adres), 2e foto 180 roteren wordt foto 3, 2e foto links roteren, wordt nieuwe foto 2 # hier was ik zo ongeveer?
  "bab2267" -> List(rotate(0,0,180,true), rotate(1,2,180,false), rotate(1,1,left,true)),
  // 2429	4e foto links roteren
  "bab2429" -> List(rotate(3,3,left,true)),
  // 1178	1e foto links roteren
  "bab1178" -> List(rotate(0,0,left,true)),
  // 92	2e foto links roteren (tekst in marge) en als 3e foto invoegen
  "bab92" -> List(rotate(1,2,left,false)),
  // 2528	3e foto rechts roteren
  "bab2528" -> List(rotate(2,2,right,true)),
  // 2290	voorlaatste foto rechts roteren en als volgende foto invoegen (tekst in marge)
  "bab2290" -> List(rotate(penultimate,last,right,false)),
  // 2343	1e foto rechts roteren (adres)
  "bab2343" -> List(rotate(0,0,right,true)),
  // 1582	foto's 1,3 en 4 links roteren
  "bab1582" -> List(∀(s => Seq(0,2,3), i => rotate(i,i,left,true))),
  // 426	3e foto rechts roteren
  "bab426" -> List(), // List(rotate(2,2,right,true)), // ### waarom? Hij staat zo op z'n kant
  // 1612	alle foto's links roteren
  "bab1612" -> List(∀(s => s.indices, i => rotate(i,i,left,true))),
  // 1550	alle foto's links roteren behalve foto 1
  "bab1550" -> List(∀(s => s.indices.drop(1), i => rotate(i,i,left,true))),
  // 278	2e foto rechts roteren
  "bab278" -> List(), // List(rotate(1,1,right,true)), // ### aangepast waarom? ligt dan verkeerd

  // 2023	1e foto 180 roteren (adres), huidige 1e foto wordt foto 3
  "bab2023" -> List(patch(0,2), rotate(0,0,180,true)),
  // 2161	volgorde foto 2 en 3 omwisselen, beide foto's ook links roteren vanwege tekst in marge; die nieuwe foto's invoegen
  "bab2161" -> List(swap(1,2),rotate(1,2,left),rotate(3,4,left)),
  // 1929	1e foto links roteren
  "bab1929" -> List(rotate(0,0,left,true)),
  // 2512	2e foto rechts roteren en na huidige 2e foto invoegen (tekst in marge)
  "bab2512" -> List(rotate(1,2,left)), // ### moet links roteren zijn, tekst zo ondersteboven
  // 2110	4e foto rechts roteren en voor huidige 4e foto plaatsen; 5e foto links roteren en voor huidige 5e foto plaatsen
  "bab2110" -> List(rotate(3,3,right),rotate(5,5,left)),
  // 1891	laatste foto 180 roteren en toevoegen
  "bab1891" -> List(rotate(last,length,180)),
  // 2504	3e foto rechts roteren en voor huidige 3e foto invoegen
  "bab2504" -> List(rotate(2,2,right)),
  // 2056	5e foto links roteren en invoegen, laatste foto 180 roteren en als laatste invoegen
  "bab2056" -> List(rotate(4,5,left),rotate(last,length,180)),
  // 1583	1e, 3e en 4e foto links roteren
  "bab1583" -> List(∀(s => Seq(0,2,3), i => rotate(i,i,left,true))),
  // 403	1e foto 180 roteren en toevoegen als 3e foto
  "bab403" -> List(rotate(0,2,180)),
  // 2520	2e en 3e foto rechts roteren
  "bab2520" -> List(∀(s => Seq(1,2), i => rotate(i,i,right,true))),
  // 2521	alle foto's rechts roteren behalve 1e foto
  "bab2521" -> List(∀(s => s.indices.drop(1), i => rotate(i,i,right,true))),
  // 2516	alle foto's rechts roteren behalve 1e foto
  "bab2516" -> List(∀(s => s.indices.drop(1), i => rotate(i,i,right,true))),
  // 2518	alle foto's rechts roteren behalve 1e foto
  "bab2518" -> List(∀(s => s.indices.drop(1), i => rotate(i,i,right,true))),
  // 2519	3e en 4e foto rechts roteren
  "bab2519" -> List(∀(s => Seq(2,3), i => rotate(i,i,right,true))),
  // 2517	alle foto's rechts roteren behalve laatstefoto
  "bab2517" -> List(∀(s => 0 until s.length-1, i => rotate(i,i,right,true))),
  // 2433	2e foto links roteren
  "bab2433" -> List(rotate(1,1,left,true)),
  // 2432	alle foto's links roteren behalve laatste foto
  "bab2432" -> List(∀(s => 0 until s.length-1, i => rotate(i,i,left,true))),
  // 2505	2e foto links roteren
  "bab2505" -> List(rotate(1,1,left,true)),
  // 2557	3e foto links roteren
  "bab2557" -> List(rotate(2,2,left,true)),
  // 2507	 3e en 5e foto links roteren
  "bab2507" -> List(rotate(2,2,left,true), rotate(4,4,left,true)),
  // 1462	1e foto links roteren en als 2e foto invoegen
  "bab1462" -> List(rotate(0,1,left)),
  // 2280	1e foto links roteren en als 2e foto invoegen
  "bab2280" -> List(rotate(0,1,left)),
  // 2446	alle foto's behalve 1e foto links roteren
  "bab2446" -> List(∀(s => s.indices.drop(1), i => rotate(i,i,left,true))),
  // 2484	alle foto's rechts roteren behalve 1e foto
  "bab2484" -> List(∀(s => s.indices.drop(1), i => rotate(i,i,right,true))),
  // 2176	1e foto rechts roteren (adres), 2e en 3e foto 180 roteren
  "bab2176" -> List(rotate(0,0,right,true), rotate(1,1,180,true), rotate(2,2,180,true)), // ### aangepast
  // 1883	2e en 3e foto rechts roteren en invoegen
  "bab1883" -> List(rotate(2,2,right),rotate(1,1,right)), // ### aangepast
  // 2314	beide foto's rechts roteren, foto 2 nog eens 180 roteren en invoegen
  "bab2314" -> List(rotate(1,1,180),rotate(0,0,right,true),rotate(2,2,right,true)), // ### aangepast
  // 2266	1e foto rechts roteren (adres), 2e foto 180 roteren
  "bab2266" -> List( rotate(0,0,right,true), rotate(1,1,180,true)), // ## aangepast
  // 1142	2 foto's ontbreken die met het slot van de brief te maken hebben: 17-06-2009 535-1 en 17-06-2009 535-2 (hierbij geleverd); textfile 17-06-2009 531-534 deleren en vervangen door  17-06-2009 531-535-2.txt
  "bab1142" -> List(),
  // 2282	1e foto rechts roteren (adres), 2e en 3e foto 180 roteren
  "bab2282" -> List(rotate(0,0,right,true), rotate(1,1,180,true), rotate(2,2,180,true)),
  // 2334	1e foto rechts roteren (adres), 2e  foto 180 roteren
  "bab2334" -> List(rotate(0,0,right,true), rotate(1,1,180,true)),
  // 2234	1e foto rechts roteren (adres), 2e  foto 180 roteren
  "bab2234" -> List(rotate(0,0,right,true), rotate(1,1,180,true)),
  // 2335	1e foto rechts roteren (adres), 2e en 3e foto 180 roteren
  "bab2335" -> List(rotate(0,0,right,true), rotate(1,1,180,true), rotate(2,2,180,true)),
  // 1959	2e foto links roteren en invoegen als 3e foto
  "bab1959" -> List(rotate(1,2,left)),
  // 2085	1e foto 180 roteren, 2e foto rechts roteren, huidige 2e foto wordt 3e foto
  "bab2085" -> List(rotate(1,1,right)), // RONDE2, rotatie 0 vervalt
  // 2083	1e foto 180 roteren (adres), 2e en 3e foto links roteren /// #waarom???? Dit klopt toch niet?
  "bab2083" -> List(), // List(rotate(0,0,180,true), rotate(1,1,left,true), rotate(2,2,left,true)), // ### aangepast
  // 2450	alle foto's behalve 1e foto links roteren
  "bab2450" -> List(∀(s => s.indices.drop(1), i => rotate(i,i,left,true))),
  // 1816	2e foto links roteren (tekst in marge) en als 3e foto invoegen
  "bab1816" -> List(rotate(1,2,left)),
  // 2143	1e foto 180 roteren (adres), 2e foto links roteren
  "bab2143" -> List(rotate(0,0,180,true), rotate(1,1,left,true)),
  // 2144	1e foto 180 roteren
  "bab2144" -> List(rotate(0,0,180,true)),

  // 2448	alle foto's links roteren
  "bab2448" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 279	1e foto rechts roteren // waarom? Komt zo verkeerd te staan
  "bab279" -> List(), // List(rotate(0,0,right,true)), // ### aangepast
  // 2145	1e foto 180 roteren, andere links roteren
  "bab2145" -> List(rotate(0,0,180,true), ∀(_.indices.drop(1), i => rotate(i,i,left,true))),
  // 1437	laatste foto 180 roteren en toevoegen
  "bab1437" -> List(rotate(last,length,180)),
  // 2515	alle foto's rechts roteren behalve laatste foto
  "bab2515" -> List(∀(_.indices.dropRight(1), i => rotate(i,i,right,true))),
  // 2514	2e foto rechts roteren
  "bab2514" -> List(rotate(1,1,right,true)),
  // 2437	2e foto links roteren
  "bab2437" -> List(rotate(1,1,left,true)),
  // 1638	alle foto's links roteren
  "bab1638" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1643	foto links roteren
  "bab1643" -> List(rotate(0,0,left,true)),
  // 2447	alle foto's links roteren
  "bab2447" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 2048	1e foto 180 graden en invoegen
  "bab2048" -> List(rotate(0,1,180)),
  // 2004	laatste foto 02-07-2010 135 ontbreekt, omdat die niet in textfile 02-07-2010 132-135 was aangegeven; hierbij nieuwe textfile  02-07-2010 132-135
  "bab2004" -> List(),
  // 2020	1e foto 180 roteren, huidige 1e foto wordt 5e foto
  "bab2020" -> List(patch(0,4),rotate(0,0,180,true)),
  // 2268	1e foto 180 roteren, 2e foto links roteren
  "bab2268" -> List(rotate(0,0,180,true), rotate(1,1,left,true)),
  // 1308	5e foto rechts roteren, 6e foto links roteren (tekst marge) en invoegen na huidige 6e foto
  "bab1308" -> List(rotate(4,4,right,true), rotate(5,6,left)),
  // 1768	2e foto links roteren (tekst in marge) en als 3e foto invoegen
  "bab1768" -> List(rotate(1,2,left)),
  // 1642	1e, 2e en laatste foto links roteren, laatste foto ook 180 graden roteren en invoegen als laatste
  "bab1642" -> List(rotate(last,length,180), rotate(0,0,left,true), rotate(1,1,left,true), rotate(penultimate,penultimate,left,true)),
  // 2308	1e foto 180 roteren, 2e en 3e foto links roteren
  "bab2308" -> List(rotate(0,0,180,true), rotate(1,1,left,true), rotate(2,2,left,true)),
  // 2451	2e foto rechts roteren, huidige 2e foto (tekst in marge) wordt 3e foto
  "bab2451" -> List(rotate(1,1,right)),
  // 1530	alle foto's links roteren
  "bab1530" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 2025	3e foto links roteren (tekst in marge)en toevoegen als 4e foto
  "bab2025" -> List(rotate(2,3,left)),
  // 2337	1e foto 180 roteren (adres); alle volgende foto's links roteren
  "bab2337" -> List(rotate(0,0,180,true), ∀(_.indices.drop(1), i => rotate(i,i,left,true))),
  // 2223	1e foto 180 roteren, 2e foto rechts roteren, huidige 2e foto wordt 3e foto (tekst in marge)
  "bab2223" -> List(rotate(0,0,180,true), rotate(1,1,right)),
  // 1908	3e foto rechts roteren, huidige 3e foto wordt 4e foto (tekst in marge)
  "bab1908" -> List(rotate(2,2,right)),
  // 1645	alle foto's links roteren
  "bab1645" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1433	laatste foto links roteren (vanwege tekst marge) en invoegen als nieuwe laatste foto
  "bab1433" -> List(rotate(last,length,left)),
  // 2445	1e en 2e foto allebei links roteren, huidige 3e foto moet 2e foto worden
  "bab2445" -> List(swap(1,2),rotate(0,0,left,true), rotate(2,2,left,true)),
  // 1528	alle foto's links roteren
  "bab1528" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 2523	4e, 5e en 6e foto rechts roteren
  "bab2523" -> List(∀(s => Seq(3,4,5), i => rotate(i,i,right,true))),
  // 1784	1e foto links roteren
  "bab1784" -> List(rotate(0,0,left,true)),
  // 1527	alle foto's links roteren
  "bab1527" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 2182	2e foto links roteren en invoegen als 3e foto, huidige 3e foto links roteren
  "bab2182" -> List(rotate(1,2,left), rotate(3,3,left,true)), // # aangepast, ongedraaide laatste was overbodig
  // 2513	2e, 3e, 4e en 6e  foto rechts roteren
  "bab2513" -> List(∀(s => Seq(1,2,3,5), i => rotate(i,i,right,true))),
  // 2162	1e foto 180 roteren, 2e en 3e foto links roteren
  "bab2162" -> List(rotate(0,0,180,true), rotate(1,1,left,true), rotate(2,2,left,true)),
  // 2276	2e foto links roteren (tekst in marge) en als 3e foto invoegen
  "bab2276" -> List(rotate(1,2,left)),
  // 1580	alle foto's links roteren
  "bab1580" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 2313	2e foto links roteren, 3e en 4e 180 roteren
  "bab2313" -> List(rotate(1,1,left,true), rotate(2,2,180,true), rotate(3,3,180,true)),
  // 1809	1e foto rechts roteren en als 3e foto invoegen
  "bab1809" -> List(rotate(0,2,right)), // ## aangepast, moest als laatste
  // 1620	2e foto links roteren
  "bab1620" -> List(rotate(1,1,left,true)),
  // 2444	2e en 3e foto links roteren
  "bab2444" -> List(rotate(1,1,left,true), rotate(2,2,left,true)),
  // 2336	1e foto rechts roteren, 2e en 3e foto 180 roteren
  "bab2336" -> List(rotate(0,0,right,true), rotate(1,1,180,true), rotate(2,2,180,true)),
  // 1619	2e en 3e foto links roteren
  "bab1619" -> List(rotate(1,1,left,true), rotate(2,2,left,true)),
  // 2157	1e foto rechts roteren, 2e foto 180 roteren
  "bab2157" -> List(rotate(0,0,right,true), rotate(1,1,180,true)),
  // 2390	1e foto links roteren en invoegen als 2e foto
  "bab2390" -> List(rotate(0,1,left)),
  // 1603	2e foto links roteren
  "bab1603" -> List(rotate(1,1,left,true)),
  // 1605	1e foto links roteren, 2e foto rechts roteren en invoegen, huidige 2e foto wordt 3e foto
  "bab1605" -> List(rotate(0,0,left,true), rotate(1,1,right)),
  // 1600	1e foto links roteren
  "bab1600" -> List(rotate(0,0,left,true)),
  // 1072	4e foto links roterenen invoegen
  "bab1072" -> List(rotate(3,4,left)),
  // 1598	alle foto's links roteren
  "bab1598" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1599	1e foto links roteren
  "bab1599" -> List(rotate(0,0,left,true)),
  // 1564	alle foto's links roteren
  "bab1564" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1652	3e foto links roteren en als 4e invoegen
  "bab1652" -> List(rotate(2,3,left)),
  // 1604	alle foto's links roteren
  "bab1604" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1591	2e foto links roteren, huidige 2e foto wordt 3e foto
  "bab1591" -> List(rotate(1,1,left)),
  // 1590	alle foto's links roteren
  "bab1590" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1474	2e foto links roteren en invoegen achter huidige 2e foto, 3e foto links roterenen invoegen achter huidige 3e foto
  "bab1474" -> List(rotate(2,3,left), rotate(1,2,left)),
  // 1551	alle foto's links roteren
  "bab1551" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1601	2e foto links roteren en invoegen als 3e foto
  "bab1601" -> List(rotate(1,2,left)),
  // 1587	alle foto's links roteren
  "bab1587" -> List(∀(_.indices, i => rotate(i,i,left,true))), // ### aangepast
  // 1475	foto's ontbreken: het zijn 05-01-2010 426a en 05-01-2010 427 hierbij geleverd
  "bab1475" -> List(),
  // 2488	2e en 3e foto rechts roteren
  "bab2488" -> List(rotate(1,1,right,true), rotate(2,2,right,true)),
  // 1592	1e foto links roteren
  "bab1592" -> List(rotate(0,0,left,true)),
  // 1562	1e en 2e foto links roteren
  "bab1562" -> List(∀(x => Seq(0,1), i => rotate(i,i,left,true))),
  // 1566	1e foto links roteren
  "bab1566" -> List(rotate(0,0,left,true)),
  // 1593	1e en 2e foto links roteren
  "bab1593" -> List(∀(x => Seq(0,1), i => rotate(i,i,left,true))),
  // 1608	alle foto's links roteren
  "bab1608" -> List(∀(_.indices, i => rotate(i,i,left,true))),
  // 1699	2e foto rechts roteren, huidige 2e foto wordt 3e
  "bab1699" -> List(rotate(1,1,right)),
  // 2486	2e foto links roteren en invoegen als 3e foto
  "bab2486" -> List(rotate(1,2,left)),
  // 1682	2e foto links roteren en invoegen als 3e foto
  "bab1682" -> List(rotate(1,2,left)),
  // 749	2e foto links roteren en invoegen als 3e foto
  "bab749" -> List(rotate(1,2,left)),
  // 1064	3e foto links roteren (tekst in marge)en toevoegen als 4e foto; laatste foto deleren // dit gaat niet goed....
  "bab1064" -> List(remove(last), remove(last), remove(last), rotate(2,3,left)), /// ### aangepast ! deze nog controleren
  // 1771	2e foto links roteren en invoegen als 3e foto
  "bab1771" -> List(rotate(1,2,left)),
  // 2348	1e foto 180 roteren, huidige 1e foto wordt 2e foto
  "bab2348" -> List(rotate(0,0,180)),
  // 2428	3e foto links roteren
  "bab2428" -> List(rotate(2,2,left,true)),
  // 759	2e foto links roteren en invoegen als 3e foto
  "bab759" -> List(rotate(1,2,left)),
  // 2559	2e foto recht roteren (tekst in marge) en invoegen als foto 3
  "bab2559" -> List(rotate(1,2,right)),
   // bab1889 alle foto's behalve de 1e, 90 links roteren
   "bab1889" -> List(∀(s => (1 until s.size), i => rotate(i,i,left,true))),  // RONDE2
 )
}
