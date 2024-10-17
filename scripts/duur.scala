  def formatTime (x: Int) = {
    val y = 60 * 60 * 1000
    val h = x / y
    val m = (x - (h * y)) / (y / 60)
    val s = (x - (h * y) - (m * (y / 60))) / 1000
    val mi = x - (h * y) - (m * (y / 60)) - (s * 1000)
    (h, m, s, mi)
    val hs = String.format("%05d", h.asInstanceOf[Object])
    val ms = String.format("%02d", m.asInstanceOf[Object])
    val ss = String.format("%02d", s.asInstanceOf[Object])
    val mis = String.format("%03d", mi.asInstanceOf[Object])
    s"$hs:$ms:$ss.$mis"
  }

  // create view transcriptie_duur as select o.transcriptie_id, max(eindtijd) as duur from transcriptie o, elan_annotatie e  where o.transcriptie_id=e.transcriptie_id group by o.transcriptie_id; 
  //
  println(formatTime(1551133799)) // 430:52:13.799
  //
  // 1206 personen select distinct persoon_id from opname__persoon where opname_functie_id=1 and opname_id in (select opname_id from transcriptie);
  // in de treebank (dus alleen geverifieerde parses: 449979 tokens,	49490 zinnen)

