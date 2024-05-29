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
      | create view  elan_annotatie_plus as select elan_annotatie.*, tagged_tokens.tokens as tagged_tokens from elan_annotatie left join tagged_tokens on tagged_tokens.elan_annotatie_id=elan_annotatie.elan_annotatie_id;
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