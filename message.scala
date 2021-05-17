import scala.util.matching.Regex
import scala.io.Source
import scala.collection.mutable.ListBuffer

class spreadsheet(
					 var L : ListBuffer[(List[String] , () => List[String])]
				 ){
	def update() : Unit = {
		for(i <- 0 until L.size)
			L = L.updated(i , (L(i)._2(), L(i)._2))
	}
}

object Demo {

	def main(args: Array[String])
	{
		// primeira coluna eh a lista com todas as palavras da file
		// a lambda da palavras le toda a file, transforma em uma string, depois filtra toda a string por um regex.
		// depois do filtro o resultado e entao transformado em uma lista de strings
		var planilha = new spreadsheet(ListBuffer[(List[String] , () => List[String])]())
		planilha.L = ListBuffer((List[String]() , () => ("[a-zA-Z]{2,}".r findAllIn Source.fromFile(
			scala.io.StdIn.readLine("Nome do Arquivo com as palavras: ")).mkString).toList))

		// segunda coluna eh a lista com todas as palavras nao permitidas
		// le a file com as palavras nao permitidas, separa elas a partir das virgulas
		// e entao transforma o array[String] em um list[String]
		planilha.L  += ((List[String]() , () => Source.fromFile(scala.io.StdIn.readLine(
			"Nome do Arquivo com as palavras nao permitidas: ")).mkString.split(",").toList))

		// terceira coluna eh a primeira coluna - segunda coluna.
		// filtra todas as palavras da file, de forma a retirar todas as palavras nao permitidas
		planilha.L  += ((List[String](), () => planilha.L(0)._1.filter(x => !planilha.L(1)._1.contains(x))))


		// quarta coluna sao todas as palavras permitidas, porem sem repeticao
		// pega todas as palavras permitidas, transforma num set, e depois transforma em list
		planilha.L  += ((List[String](), () => planilha.L(2)._1.toSet.toList))

		// quinta coluna e uma lista com o numero de ocorrencia das palavras na terceira coluna
		// itera por toda a lista de palavras da quarta coluna e, em cada iteracao, checa a qtd de vezes
		// tal palavra aparece na lista da terceira coluna.
		planilha.L  += ((List[String](), () => (for(x <- planilha.L (3)._1.toList)
			yield planilha.L (2)._1.count(y => y.equals(x)).toString)))
			
		// sexta coluna e uma lista ordenada decrescentemente baseada na qtd de vezes que tal palavra aparece
		// inicialmente se juntam as listas da quarta e quinta colunas com o metodo zip.
		// ordenamos entao a lista resultante baseado no counter da palavra.
		// entao mapeamos a lista ordenada em uma lista de string.
		planilha.L  += ((List[String]() , () => (planilha.L (3)._1.toList zip planilha.L(4)._1
			).sortWith((a,b) => a._2.toInt > b._2.toInt).map(x => x._1 + " -> " + x._2)))

		for(i <- planilha.L)
			println(i._1)

		while(true)
		{
			planilha.update

			for(i <- planilha.L)
				println(i._1)
		}

	}
}