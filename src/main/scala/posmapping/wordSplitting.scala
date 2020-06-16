package posmapping

import corpusprocessing.CRM.readMTASMappingForCRM._


import scala.xml._

// dit verandert de tags; die moet dus wel even gevalideerd worden....
object wordSplitting {

  val squareCup = "⊔"

  def splitW(w: Elem): NodeSeq = // lelijk: we verliezen de tagging van expan enzo....
  {
    val word = (w \ "seg").text.trim
    val msdValue = (w \ "@msd").text
    val posValue = (w \ "@pos").text
    val id = w.attributes.filter(_.key == "id").head.value.text

    val parts = word.split(s"(\\s+|$squareCup)")

    if (parts.size == 1)
      {
        Seq(w)
      } else { // TODO je moet ook nog corresp zetten en de
      parts.toSeq.zipWithIndex.map({ case (p,i) =>

          val newSeg = <seg>{p}</seg>


          val partClass = if (i == 0) "deel-b" else if (i == parts.size - 1) "deel-f" else "deel-i"
          val partClassEn = if (i == 0) "part-s" else if (i == parts.size - 1) "part-f" else "part-i"
          val newId = new PrefixedAttribute("xml", "id", id + "." + i.toString, Null)

          val newMsdValue = msdValue.replaceAll("\\)", s",$partClass)").replaceAll("\\(,", "(")
          val newPosValue =  posValue.replaceAll("\\)", s",part=$partClassEn)").replaceAll("\\(,", "(")
          val check = newMsdValue.split("\\+").map(CGNMiddleDutchTag(_)) // moet wel even gecheckt!!!

          val corresp = parts.indices.filter( _ != i).map(j => "#" + id + "." + j.toString).mkString(" ")
          val lemmaRef = parts.indices.map(j => "#" + id + "." + j.toString).mkString(" ")

          val newAtts = w.attributes.filter(x => !(x.key == "msd" || x.key == "id" || x.key == "pos"))
            .append(newId)
            .append(new UnprefixedAttribute("msd",newMsdValue,Null))
            .append(new UnprefixedAttribute("pos",newPosValue,Null))
            .append(new UnprefixedAttribute("corresp", corresp, Null))
            .append(new UnprefixedAttribute("lemmaRef", lemmaRef, Null))


          val newChild = newSeg ++ w.child.filter(c => !(c.label == "seg"))

          val pw =w.copy(child=newChild, attributes = newAtts)

          val featureStructures: Seq[Elem]= (w \ "fs").map(_.asInstanceOf[Elem])

          val fzNew: Seq[Elem] = featureStructures.map(fs => {

            val typ = (fs \ "@type").text
            if (typ == "gys")
              {
                fs.copy(child = fs.child ++  Seq(<f name="part"><symbol value={partClassEn}/></f>))
              }  else {
              fs.copy(child = fs.child ++  Seq(<f name="deel"><symbol value={partClass}/></f>))
            }
          })

          val fsNew = if (featureStructures.nonEmpty)
            {
              val hd = featureStructures.head.asInstanceOf[Elem]
              Seq(hd.copy(child = hd.child ++ Seq(<f name="deel"><symbol value={partClass}/></f>))) ++ featureStructures.tail }
          else
            Seq()

            // Console.err.println(pw)
          pw.copy(child = pw.child.filter(c => !(c.label == "fs")) ++ fzNew)
        }
      )
    }
  }

  import utils.PostProcessXML._

  def splitWords(d: Elem):Elem = updateElement2(d, _.label=="w", splitW).asInstanceOf[Elem]


}
