import Formula
import Parser
import BDD



main :: IO()
main = do
	let f = parseFormula "P | (Q & R)"
	let bdd = constructBDD ["P", "Q", "R"] f
	putStr . prettyPrintBDD $ bdd
