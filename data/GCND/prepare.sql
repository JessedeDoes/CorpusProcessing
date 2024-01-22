start transaction;
drop view if exists elan_2_alpino cascade;
drop view if exists elan_2_alpino_unique;
create view elan_2_alpino as

	select e.transcriptie_id, elan_annotatie_id, alpino_annotatie_id 
	from elan_annotatie e, alpino_annotatie a 
	where e.transcriptie_id = a.transcriptie_id and 
	(a.starttijd >= e.starttijd and a.starttijd < e.eindtijd) and (a.eindtijd > e.starttijd and a.eindtijd <= e.eindtijd);
	create view elan_2_alpino_unique as select transcriptie_id, alpino_annotatie_id, max(elan_annotatie_id) as elan_annotatie_id from elan_2_alpino group by alpino_annotatie_id, transcriptie_id;
	select * from elan_2_alpino_unique order by alpino_annotatie_id;
commit;
