package eindhoven

object pronominabel {
  val gradables = """veel
    weinig
    beide
    meer
    teveel
    minder
    keiveel
    meeste
    evenveel
    zoveel
    mindere
    vele
    superveel
    keiweinig
    allerminst
    minst
    veels
    zovele
    vaak""".split("\\s+").toSet // waarom 'beide' grad??
}


/*
[T501a]	VNW(pers,pron,nomin,vol,1,ev)	ik
[T501b]	VNW(pers,pron,nomin,nadr,1,ev)	ikzelf, ikke
[T501c]	VNW(pers,pron,nomin,red,1,ev)	’k
[T501d]	VNW(pers,pron,nomin,vol,1,mv)	wij
[T501e]	VNW(pers,pron,nomin,nadr,1,mv)	wijzelf
[T501f]	VNW(pers,pron,nomin,red,1,mv)	we
[T501g]	VNW(pers,pron,nomin,vol,2v,ev)	jij
[T501h]	VNW(pers,pron,nomin,nadr,2v,ev)	jijzelf
[T501i]	VNW(pers,pron,nomin,red,2v,ev)	je
[U501j]	VNW(pers,pron,nomin,vol,2b,getal)	u
[U501k]	VNW(pers,pron,nomin,nadr,2b,getal)	uzelf
[U501l]	VNW(pers,pron,nomin,vol,2,getal)	gij
[U501m]	VNW(pers,pron,nomin,nadr,2,getal)	gijzelf
[U501n]	VNW(pers,pron,nomin,red,2,getal)	ge

[U501o]	VNW(pers,pron,nomin,vol,3,ev,masc)	hij
[T501p]	VNW(pers,pron,nomin,nadr,3m,ev,masc)	hijzelf
[U501q]	VNW(pers,pron,nomin,red,3,ev,masc)	ie
[U501r]	VNW(pers,pron,nomin,red,3p,ev,masc)	men
[T501s]	VNW(pers,pron,nomin,vol,3v,ev,fem)	zij
[T501t]	VNW(pers,pron,nomin,nadr,3v,ev,fem)	zijzelf
[U501u]	VNW(pers,pron,nomin,vol,3p,mv)	zij
[U501v]	VNW(pers,pron,nomin,nadr,3p,mv)	zijzelf
[T502a]	VNW(pers,pron,obl,vol,2v,ev)	jou
[U502b]	VNW(pers,pron,obl,vol,3,ev,masc)	hem
[T502c]	VNW(pers,pron,obl,nadr,3m,ev,masc)	hemzelf
[U502d]	VNW(pers,pron,obl,red,3,ev,masc)	’m
[U502e]	VNW(pers,pron,obl,vol,3,getal,fem)	haar
[U502f]	VNW(pers,pron,obl,nadr,3v,getal,fem)	haarzelf
[U502g]	VNW(pers,pron,obl,red,3v,getal,fem)	’r, d’r
[U502h]	VNW(pers,pron,obl,vol,3p,mv)	hen, hun
[U502i]	VNW(pers,pron,obl,nadr,3p,mv)	henzelf, hunzelf
[U503a]	VNW(pers,pron,stan,nadr,2v,mv)	jullie
[U503b]	VNW(pers,pron,stan,red,3,ev,onz)	het, ’t
[U503c]	VNW(pers,pron,stan,red,3,ev,fem)	ze
[U503d]	VNW(pers,pron,stan,red,3,mv)	ze
[T504a]	VNW(pers,pron,gen,vol,1,ev)	mijns gelijke, gedenk mijner
[T504b]	VNW(pers,pron,gen,vol,1,mv)	ons gelijke, velen  onzer
[U504c]	VNW(pers,pron,gen,vol,2,getal)	uws gelijke, wie  uwer
[T504d]	VNW(pers,pron,gen,vol,3m,ev)	zijns gelijke,  zijner
[U504e]	VNW(pers,pron,gen,vol,3v,getal)	haars gelijke, harer
[U504f]	VNW(pers,pron,gen,vol,3p,mv)	huns gelijke, een  hunner
 */