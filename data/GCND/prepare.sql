start transaction;
drop view if exists elan_2_alpino cascade;
drop view if exists elan_2_alpino_unique;
drop view if exists unused_alpino_annotations;
drop view if exists n_tokens_elan;
drop view if exists n_tokens_alpino;

create view elan_2_alpino as

	select e.transcriptie_id, elan_annotatie_id, alpino_annotatie_id, 
	case  
	  when a.starttijd >= e.starttijd and a.starttijd < e.eindtijd then least(e.eindtijd,a.eindtijd) - a.starttijd
	  when a.eindtijd > e.starttijd and a.eindtijd <= e.eindtijd then a.eindtijd - greatest(a.starttijd, e.starttijd)
	  else 0 
	end as overlap
	from elan_annotatie e, alpino_annotatie a 
	where 
	  e.transcriptie_id = a.transcriptie_id and 
	  ((a.starttijd >= e.starttijd and a.starttijd < e.eindtijd) or (a.eindtijd > e.starttijd and a.eindtijd <= e.eindtijd));

	create view alpino_2_elan_unique as select 
		transcriptie_id, 
		alpino_annotatie_id, 
		(array_agg(elan_annotatie_id order by overlap desc))[1] as elan_annotatie_id, 
		array_agg(elan_annotatie_id order by overlap desc) as options,
		array_agg(overlap order by overlap desc) as overlaps from elan_2_alpino group by alpino_annotatie_id, transcriptie_id;
	-- select * from elan_2_alpino_unique order by alpino_annotatie_id;

   
 

   create view unused_alpino_annotations as select transcriptie_id, alpino_annotatie_id from alpino_annotatie 
		where not alpino_annotatie_id in (select alpino_annotatie_id from alpino_2_elan_unique);

    
   create view n_tokens_elan as select transcriptie_id, elan_annotatie_id, JSON_ARRAY_LENGTH(tokens::json) as n_tokens from elan_annotatie;
   create view n_tokens_alpino as select transcriptie_id, alpino_annotatie_id, JSON_ARRAY_LENGTH(tokens::json) as n_tokens from alpino_annotatie;
   

   create  view elan_2_alpinos as select 
     e.elan_annotatie_id, array_agg(e.alpino_annotatie_id order by  e.alpino_annotatie_id) as alpino_annotatie_ids, array_agg(n_tokens order by  a.alpino_annotatie_id) 
     as n_alpino_tokens
   from alpino_2_elan_unique e, n_tokens_alpino a where e.alpino_annotatie_id = a.alpino_annotatie_id group by e.elan_annotatie_id;

 -- (SELECT SUM(s) FROM UNNEST(monthly_usage) s) as total_usage
   create view n_tokens as select
     e.elan_annotatie_id, JSON_ARRAY_LENGTH(tokens::json) as n_elan_tokens, ( select sum(x) from unnest(n_alpino_tokens) x)  as n_alpino_tokens
   from elan_annotatie e left join elan_2_alpinos x on e.elan_annotatie_id = x.elan_annotatie_id; 


  select * from n_tokens;

   
   -- select * from unused_alpino_annotations;
abort;
