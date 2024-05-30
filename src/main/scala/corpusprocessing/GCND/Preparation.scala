package corpusprocessing.GCND

object Preparation {
  val queries =
    """start transaction;
      |drop view if exists elan_2_alpino cascade;
      |drop view if exists elan_2_alpino_unique;
      |create view elan_2_alpino as
      |
      |	select e.transcriptie_id, elan_annotatie_id, alpino_annotatie_id,
      |	case
      |	  when a.starttijd >= e.starttijd and a.starttijd < e.eindtijd then least(e.eindtijd,a.eindtijd) - a.starttijd
      |	  when a.eindtijd > e.starttijd and a.eindtijd <= e.eindtijd then a.eindtijd - greatest(a.starttijd, e.starttijd)
      |	  else 0
      |	end as overlap
      |	from elan_annotatie e, alpino_annotatie a
      |	where e.transcriptie_id = a.transcriptie_id and
      |	((a.starttijd >= e.starttijd and a.starttijd < e.eindtijd) or (a.eindtijd > e.starttijd and a.eindtijd <= e.eindtijd));
      |
      |	create view alpino_2_elan_unique as select
      |		transcriptie_id,
      |		alpino_annotatie_id,
      |		(array_agg(elan_annotatie_id order by overlap desc))[1] as elan_annotatie_id,
      |		array_agg(elan_annotatie_id order by overlap desc) as options,
      |		array_agg(overlap order by overlap desc) as overlaps from elan_2_alpino group by alpino_annotatie_id, transcriptie_id;
      | create table if not exists tagged_tokens (elan_annotatie_id integer, tokens text);
      | drop view if exists elan_annotatie_plus;
      | create view  elan_annotatie_plus as select elan_annotatie.*, tagged_tokens.tokens as tagged_tokens
      |  from elan_annotatie left join tagged_tokens on tagged_tokens.elan_annotatie_id=elan_annotatie.elan_annotatie_id;
      |
      |drop view if exists persoon_persoon_plus cascade;
      |create view persoon_persoon_plus as
      |select
      |   persoon__persoon.*,
      |   relatie_type.label
      |   from persoon__persoon, relatie_type
      |   where persoon__persoon.relatie_id = relatie_type.relatie_type_id;
      |
      |create view persoon_persoon_plusplus
      |as select
      |   persoon_persoon_plus.*,
      |   plaats1.plaats_id as plaats_id1,
      |   plaats1.kloeke_code as kloeke_code1,
      |   plaats1.provincie_id as provincie_id1,
      |   plaats1.dialectgebied_id as dialectgebied_id1,
      |   persoon1.gender_id as gender_id1,
      |
      |   plaats2.plaats_id as plaats_id2,
      |   plaats2.kloeke_code as kloeke_code2,
      |   plaats2.provincie_id as provincie_id2,
      |   plaats2.dialectgebied_id as dialectgebied_id2,
      |   persoon2.gender_id as gender_id2
      |from
      |   persoon_persoon_plus,
      |   persoon persoon1,
      |   persoon persoon2,
      |   plaats plaats1,
      |   plaats plaats2
      |where
      |   persoon_persoon_plus.persoon_id1=persoon1.persoon_id and
      |   persoon_persoon_plus.persoon_id2=persoon2.persoon_id and
      |   persoon1.geboorte_plaats_id=plaats1.plaats_id and
      |   persoon2.geboorte_plaats_id=plaats2.plaats_id
      |;
      |create view persoon_persoon_plusplusplus as
      |select
      |  persoon_persoon_plusplus.*,
      |  plaats_id1 = plaats_id2 as plaats_match,
      |  provincie_id1 = provincie_id2 as provincie_match,
      |  dialectgebied_id1 = dialectgebied_id2 as dialectgebied_match
      |from
      |  persoon_persoon_plusplus;
      |
      |-- select * from persoon_persoon_plusplusplus;
      |
      |create view persoon_persoon_plusplusplusplus as select
      |   persoon_id1,
      |   persoon_id2,
      |   plaats_match,
      |   dialectgebied_match,
      |   provincie_match,
      |   label,
      |   gender_id1,
      |   gender_id2,
      |   case
      |     when plaats_match then 'zelfde plaats'
      |     when provincie_match then 'zelfde provincie'
      |     -- when dialectgebied_match then 'zelfde dialectgebied'
      |     else 'andere provincie'
      |   end as match_level
      |   from persoon_persoon_plusplusplus; --  where provincie_match or dialectgebied_match;
      |
      |create view persoon_vaderplaats as select  distinct
      |   persoon_id1,
      |   persoon_id2,
      |   plaats_match,
      |   dialectgebied_match,
      |   provincie_match,
      |   match_level
      |   from persoon_persoon_plusplusplusplus
      |where label ~ 'ouder' and gender_id1=1;
      |
      |create view persoon_moederplaats as select distinct
      |    persoon_id1,
      |    persoon_id2,
      |    plaats_match,
      |    dialectgebied_match,
      |    provincie_match,
      |    match_level
      |    from persoon_persoon_plusplusplusplus
      |where label ~ 'ouder' and gender_id1=2;
      |
      |create view persoon_partnerplaats as select distinct
      |    persoon_id1,
      |    persoon_id2,
      |    plaats_match,
      |    dialectgebied_match,
      |    provincie_match,
      |    match_level
      |    from persoon_persoon_plusplusplusplus
      |where label ~ 'partner';
      |commit;
      |""".stripMargin.split(";")
}


/*
create view persoon_persoon_plus as select persoon__persoon.*,
relatie_type.label from persoon__persoon, relatie_type where persoon__persoon.relatie_id = relatie_type.relatie_type_id;

create view create view persoon_persoon_plusplus
as select persoon_persoon_plus.*, plaats.*,persoon.gender_id from persoon_persoon_plus, persoon,
plaats where persoon_persoon_plus.persoon_id2=persoon_id and persoon.geboorte_plaats_id=plaats.plaats_id;

create view persoon_vaderplaats as select
    persoon_id1 as persoon_id, plaats_id, kloeke_code, provincie_id,
    dialectgebied_id from persoon_persoon_plusplus where label='ouder van' and gender_id=1;


 */