package zeeuws.xml2db

object Mappings {

  import Classes._


  val fieldMappingLemmata = Seq[(String, Lemma => String)](
    //
    ("lemma_id", l => l.sense_id),
    ("lemma_wzd", l => l.lemma_wzd),
    ("lemma", l => l.lemma),
    ("definition", l => l.definition)
  )

  // keywords Keyword(lemma_id: String, keyword: String, kw_id: String)

  val fieldMappingKeywords = Seq[(String, Keyword => String)](
    ("lemma_id", r => r.lemma_id),
    ("keyword", r => r.keyword),
    ("keyword_id", r => r.keyword_id)
  )

  // responses   case class Response(keyword_id: String, keyword: String, place: String, isRegion: Boolean)

  val fieldMappingResponses = Seq[(String, Response => String)](
    ("keyword_id", r => r.keyword_id),
    ("keyword", r => r.keyword),
    ("place", r => r.place),
    ("isRegion", r => r.isRegion.toString)
  )

  def insert[T](db: database.Database, tableName: String, fieldMapping: Seq[(String, T => String)], things: Seq[T], recreate: Boolean = true) = {
    if (recreate) {
      db.runStatement(s"drop table if exists $tableName cascade")
      val fieldDef = fieldMapping.map(_._1).map(s => s"$s text").mkString(", ")
      db.runStatement(s"create table $tableName ($fieldDef)")
    }
    val fields = fieldMapping.map(_._1)
    def createBindings(l: T): Seq[db.Binding] = fieldMapping.map({ case (n, f) => db.Binding(n, f(l)) })
    val query = s"insert into $tableName (${fields.mkString(", ")}) values (${fields.map(":" + _).mkString(", ")})"
    val z = db.QueryBatch[T](query, t => createBindings(t))
    z.insert(things)
  }
}
