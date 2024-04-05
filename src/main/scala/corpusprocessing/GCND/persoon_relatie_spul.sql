-- tabel persoon__persoon is leeg, dus dit wordt allemaal niets
select
  p.*, q.*, reltype.*
from
  persoon p,
  persoon__persoon r,
  persoon q,
  relatie reltype
  where
   (p.persoon_id=r.persoon_id1
   and q.persoon_id=r.persoon_id2  or q.persoon_id=r.persoon_id1 and p.persoon_id=r.persoon_id2)
   and reltype.relatie_id=r.relatie_id;

select
  p.persoon_id,
  reltype.label,
  plaats_q.plaats_id=plaats_p.plaats_id as zelfde_plaats,
  plaats_q.regio_id=plaats_p.regio_id as zelfde_provincie
from
  persoon p,
  plaats plaats_p,
  persoon__persoon r,
  persoon q,
  plaats plaats_q,
  relatie reltype
  where
   (p.persoon_id=r.persoon_id1
   and q.persoon_id=r.persoon_id2  or q.persoon_id=r.persoon_id1 and p.persoon_id=r.persoon_id2)
   and p.geboorte_plaats_id=plaats_p.plaats_id
   and q.geboorte_plaats_id=plaats_q.plaats_id
   and reltype.relatie_id=r.relatie_id;
