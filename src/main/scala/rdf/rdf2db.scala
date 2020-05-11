package rdf

import ch.qos.logback.classic.db.names.TableName
import database.{Configuration, Database}
import org.openrdf.model.Statement

object rdf2db {
  def insertToTable(d: Database, tableNamex: String, statements: Stream[Statement]) = {

    val (schema, tableName) = if (tableNamex.contains("."))
      (tableNamex.replaceAll("[.].*", ""),tableNamex.replaceAll(".*[.]", ""))
    else ("public",tableNamex)


    if (schema != "public") {
      d.runStatement(s"drop schema if exists $schema cascade")
      d.runStatement(s"create schema $schema")
      d.runStatement(s"set schema '$schema'")
    }


    d.runStatement(s"drop table if exists $tableName")
    d.runStatement(s"create table $tableName (subject text, predicate text, object text)")

    def createBindings(s: Statement) =
      Seq(d.Binding("subject", s.getSubject.stringValue()),
        d.Binding("predicate", s.getPredicate.stringValue()),
        d.Binding("object", s.getObject.stringValue())
      )

    val z : Statement => Seq[d.Binding]  = s => createBindings(s)
    val qb = d.QueryBatch(s"insert into $tableName (subject,predicate,object) values (:subject, :predicate, :object)", z )
    qb.insert(statements)
    createSchema(d, s"$schema.$tableName")
  }





  def createSchema(d: Database, tableName: String): Unit =
  {
    d.runStatement("drop schema if exists schema cascade")
    d.runStatement("create schema schema")
    val statements =
      s"""
         |set schema 'schema';
         |create table resource_type as select  subject as resource, object as type from $tableName where predicate ~ 'ns#type';
         |create table distinct_types as select type, sum(1) as count from resource_type group by type;
         |create table properties as select predicate, sum(1) as count from $tableName group by predicate;
         |create table property_domains as select type, predicate, sum(1) as count from resource_type, $tableName where resource_type.resource=evoke.subject group by type,predicate;
         |create table property_ranges as select type, predicate, sum(1) as count from resource_type, $tableName where resource_type.resource=evoke.object group by type,predicate;
         |create table untyped_objects as select distinct object as resource, cast('unknown' as text) as type
         |    from $tableName where not (object in (select resource from resource_type));
         | create table untyped_subjects as select distinct subject as resource, cast('unknown' as text) as type
         |    from $tableName where not (subject in (select resource from resource_type));
         | update untyped_objects set type='literal' where not (resource ~ '://');
         | insert into resource_type select * from untyped_subjects;
         | insert into resource_type select * from untyped_objects where not (resource in (select resource from untyped_subjects));
         | create index subject_index on $tableName(subject);
         | create index object_index on $tableName(object);
         | create index rt_index on resource_type(resource);
         | create table property_types_grouped as select array_agg(distinct st.type) as subject_type, predicate, array_agg(distinct ot.type) as object_type, sum(1) as count from $tableName d, resource_type st, resource_type ot where d.subject=st.resource and d.object=ot.resource  group by predicate order by predicate;
         | create table property_types_ungrouped as select st.type as subject_type, predicate, ot.type as object_type, sum(1) as count from $tableName d, resource_type st, resource_type ot where d.subject=st.resource and d.object=ot.resource  group by st.type, ot.type, predicate order by predicate;
         |""".stripMargin.split(";")
    statements.foreach(s => {
      println(s)
      d.runStatement(s)
    })
  }



  def main(args: Array[String]): Unit = {
    //createSchema(evoke_db,"data.evoke")
  }
}

