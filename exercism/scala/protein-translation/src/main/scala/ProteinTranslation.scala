object ProteinTranslation {
  def proteins(rna: String): Seq[String] =
    rna
      .grouped(3)
      .toSeq
      .map(codon =>
        codon match {
          case "AUG"                         => "Methionine"
          case "UUU" | "UUC"                 => "Phenylalanine"
          case "UUA" | "UUG"                 => "Leucine"
          case "UCU" | "UCC" | "UCA" | "UCG" => "Serine"
          case "UAU" | "UAC"                 => "Tyrosine"
          case "UGU" | "UGC"                 => "Cysteine"
          case "UGG"                         => "Tryptophan"
          case "UAA" | "UAG" | "UGA"         => "STOP"
        }
      )
      .takeWhile(_ != "STOP")
}
