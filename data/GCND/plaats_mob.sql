select * from persoon where alias='H018p_1_2';
select * from plaats where plaats_id=1796;
select * from persoon__woonplaats where persoon_id=34;

select * from mobiliteit;
start transaction;

create view persoon_plaatsen as select 
persoon.*,
geboorteplaats.naam as geboorteplaats,
woonplaats.naam as woonplaats,
beroepplaats.naam as beroepplaats
from persoon, 
plaats geboorteplaats,
persoon__woonplaats,
plaats woonplaats,
persoon__beroepplaats,
plaats beroepplaats
where
  persoon.geboorte_plaats_id=geboorteplaats.plaats_id and
  persoon.persoon_id = persoon__woonplaats.persoon_id and persoon__woonplaats.plaats_id=woonplaats.plaats_id and
  persoon.persoon_id = persoon__beroepplaats.persoon_id and persoon__beroepplaats.plaats_id=beroepplaats.plaats_id;
  

select alias,  geboorteplaats, woonplaats, beroepplaats, woonplaats_mobiliteit_id, beroep_mobiliteit_id  from persoon_plaatsen where woonplaats=geboorteplaats and beroepplaats=geboorteplaats and (woonplaats_mobiliteit_id != 1 or beroep_mobiliteit_id !=1);
rollback;

