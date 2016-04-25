package design

import java.io.{FileNotFoundException, File}

import il.ac.technion.cs.ssdl.java.{Token, Tokenizer}

object TokenizerScala {
	private val badTokens = Set(Token.EOF,
		Token.SPACE,
		Token.NL,
		Token.DOC_COMMENT,
		Token.LINE_COMMENT,
		Token.NL_BLOCK_COMMENT,
		Token.NL_DOC_COMMENT,
		Token.PARTIAL_BLOCK_COMMENT,
		Token.PARTIAL_DOC_COMMENT,
		Token.BLOCK_COMMENT,
		Token._private,
		Token._public,
		Token._protected,
		Token._final,
		Token._assert,
		Token._volatile
	)
	def main(args: Array[String]) {
		tokenize(Array("C:/test.log"))
	}
	def tokenize(args: Array[String]) {
		try {
			val file: File = new File(args(0))
			val tokenizer: Tokenizer = new Tokenizer(file)
			val sb = new StringBuilder
			var count: Int = 0
			var token = tokenizer.next()
			while (token != Token.EOF) {
				if (badTokens(token) == false) {
					count += (if (badTokens(token)) 0 else 1)
					sb append tokenizer.text()
				}
				token = tokenizer.next()
			}
			tokenizer.closeReader()

			val entireString = sb.toString
			count -= """\(\)\s*->""".r.findAllIn(entireString).size
			count -= "->".r.findAllIn(entireString).size
			System.out.println(s"There are $count tokens in your file")
		} catch {
			case e: FileNotFoundException => {
				System.err.println("could not find file " + args(0))
			}
			case e: ArrayIndexOutOfBoundsException => {
				System.err.println("The first argument to this program should be a path to a java file")
			}
		}
	}
}
