package eval

/**
 * singleton Lambda : tous le langage est defini dans cet objet unique 
 */
object Lambda{

	private var renamed = false;

	def isRenamed():Boolean =
		{
			return renamed;
		}

	def setRenamed(bool:Boolean):Unit =
		{
			renamed = bool;
		}

	def fullEvaluation(expr:Expression):Unit ={
	  var oldExpr:Expression = null  
	  var newExpr:Expression = expr
	  while (newExpr!=oldExpr)
	  {
		  oldExpr = newExpr;
		  newExpr = newExpr.evaluate();
	  }
	}
  
	/** classe abstraite Expression*/
	abstract class Expression{

		def evaluate():Expression;

		def evaluateConversion():Expression;

		def evaluateReduction():Expression;

		def freeVariables():List[Variable];

		def allVariables():List[Variable];

		def alphaConversion(boundVar:Variable, expression:Expression, change:Boolean):Expression;

		def betaReduction(variable:Variable, body:Expression):Expression;

	};

	/** classe concrete Variable*/
case class Variable(value:String) extends Expression{
  
	private var variableValues = createVariableValues();

	/** evaluation initiale pour determiner le type d'operation a realiser*/
	override def evaluate():Expression =
	{
	  		if (!isRenamed())
	  		{
	  			var expr:Expression = this.evaluateConversion()
	  			if (expr==this)
	  			{
	  				return this.evaluateReduction();
	  			}
	  			else
	  			{
	  				Lambda.setRenamed(true);
	  				return expr
	  			}
	  		}
	  		else{
	  			Lambda.setRenamed(false);
	  			return this.evaluateReduction();
	  		}
	}

	/**evaluation d'une variable*/
	override def evaluateConversion():Expression =
		{
			//une variable s'evalue en elle meme
			return this
		}

	/**evaluation d'une variable*/
	override def evaluateReduction():Expression =
		{
			//une variable s'evalue en elle meme
			return this
		}		

	/** renommage d'une variable */
	override def alphaConversion(boundVar:Variable, expression:Expression, change:Boolean):Expression =
		{
			//si c'est bien la variable qu'on veut remplacer on la remplace par l'expression		
			if ((boundVar==this)&&(change))
			{
				return expression;
			}
			//sinon on n'y touche pas
			else
			{
				return this;
			}
		}		

	/** remplacement d'une variable par une expression */
	override def betaReduction(variable:Variable, body:Expression):Expression =
		{
			//si c'est bien la variable qu'on veut remplacer on la remplace par l'expression		
			if (variable==this)
			{
				return body;
			}
			//sinon on n'y touche pas
			else
			{
				return this;
			}
		}

	/**liste des variables libres*/
	override def freeVariables():List[Variable] =
	{
			var vars:List[Variable]= List.empty;
			vars.+:=(this);
			return vars;
	}

	override def allVariables():List[Variable] =
	{
			var vars:List[Variable]= List.empty;
			vars.+:=(this);
			return vars;
	}	

	/**cree la liste des variables possibles */
	def createVariableValues():List[String]=
		{
			var list:List[String]= List.empty;
			for (ch <- 'a' to 'z' ){
				list:+=(ch+"");
			};
			list.-=("l");
			return list;
		}	

	/** renomme une variable a l'aide de la liste des variables libres */
	def renameVariable(freeVariables:List[Variable]):Variable =
		{
			var values:List[String] = List.empty;
			values.+:=(this.value);
			freeVariables.foreach((v:Variable) => values.+:=(v.value))
			//on elimine les variables interdites et on prend la premiere lettre parmis les variables restantes
			val newValue:String = (variableValues.--(values)).first
			return new Variable(newValue);
		}

	/** affichage */
	override def toString():String ={
			return "Variable("+this.value+")"
	}

}

/** classe concrete Function*/
case class Function(parameter:Variable, body:Expression) extends Expression{

	/** evaluation initiale pour determiner le type d'operation a realiser*/
	override def evaluate():Expression =
	{
	  		if (!isRenamed())
	  		{
	  			var expr:Expression = this.evaluateConversion()
	  			if (expr==this)
	  			{
	  				return this.evaluateReduction();
	  			}
	  			else
	  			{
	  				Lambda.setRenamed(true);
	  				return expr
	  			}
	  		}
	  		else{
	  			Lambda.setRenamed(false);
	  			return this.evaluateReduction();
	  		}
	}

	/**evaluation d'une variable*/
	override def evaluateConversion():Expression =
		{
			val newBody = this.body.evaluateConversion();

			if (newBody == this.body)
			{
				return this
			}
			return new Function(this.parameter, newBody)
		}

	/**evaluation d'une variable*/
	override def evaluateReduction():Expression =
		{
			val newBody = this.body.evaluateReduction();

			if (newBody == this.body)
			{
				return this
			}
			return new Function(this.parameter, newBody)
		}		
  
	override def alphaConversion(boundVar:Variable, expression:Expression, change:Boolean):Expression =
	{
			//cas ou on essairai de remplacer une variable liee dans une sous fonction possédant une variable liee du meme nom
			if (boundVar.value == this.parameter.value)
			{
				return this
			};

			//la liste des variables libres de l'expression a appliquer
			var freeVars:List[Variable] = expression.freeVariables();

			//la liste des noms des variables libres de l'expression a appliquer
			var freeVals:List[String] = List.empty;
			freeVars.foreach((v:Variable) => freeVals.:+=(v.value));

			//le corps de la fonction apres renommage eventuel
			var newBody:Expression = this.body;

			//le parametre de la fonction apres renommage eventuel
			var newParameter:Variable = this.parameter;

			//si la variable liee de la fonction appartient aux variables libres du terme a appliquer
			if (freeVals.contains(this.parameter.value))
			{
				//on renomme la variable liee avec un nom de variable non utilise
				newParameter = this.parameter.renameVariable(freeVars);
				//on lance le renommage pour le corps de la fonction avec la nouvelle variable
				newBody = newBody.alphaConversion(this.parameter,newParameter,true);
				//le fils sait qu'un renommage est en cours
			}

			newBody = newBody.alphaConversion(boundVar,expression,change)

			//si il n'y a aucune modification a effectuer, on renvoie notre objet lui meme
			if (newBody==this.body && newParameter == this.parameter)
			{
				return this;
			};

			//sinon on renvoie l'objet modifie
			return new Function(newParameter, newBody);
	}

		override def betaReduction(variable:Variable, expression:Expression):Expression =
		{
			//cas ou on essairai de remplacer une variable liee dans une sous fonction possédant une variable liee du meme nom
			if (variable.value == this.parameter.value)
			{
				return this
			};

			//on lance la substitution sur le corps de la fonction
			var newBody:Expression = this.body.betaReduction(variable,expression);

			//si il n'y a aucune substitution, on renvoie l'objet lui meme
			if (newBody==this.body)
			{
				return this;
			};

			//sinon on renvoie l'objet modifie
			return new Function(this.parameter, newBody);
	}

	//on effectue le renomage de l'expression gauche a l'aide de l'expression droite
	def applyConversion(expr:Expression):Expression =
		{
			return new Function(this.parameter,this.body.alphaConversion(this.parameter, expr, false));
		}

	//on applique l'expression droite de l'application a  la fonction
	def applyReduction(expr:Expression):Expression =
		{
			return this.body.betaReduction(this.parameter, expr);
		}

	//liste des variables libres 
	override def freeVariables():List[Variable] =
		{
			return this.body.freeVariables().--(this.parameter.freeVariables());
		}

	override def allVariables():List[Variable] =
	{
			return this.body.allVariables();	
	}

	//affichage
	override def toString():String =
		{
			return "Function("+this.parameter.toString()+","+this.body.toString()+")"
		}	

}

/** classe concrete Application*/
case class Application(exprLeft:Expression, exprRight:Expression) extends Expression{	  

	/** evaluation initiale pour determiner le type d'operation a realiser*/
	override def evaluate():Expression =
	{
	  		if (!isRenamed())
	  		{
	  			var expr:Expression = this.evaluateConversion()
	  			if (expr==this)
	  			{
	  				return this.evaluateReduction();
	  			}
	  			else
	  			{
	  				Lambda.setRenamed(true);
	  				return expr
	  			}
	  		}
	  		else{
	  			Lambda.setRenamed(false);
	  			return this.evaluateReduction();
	  		}
	}

	/** alpha-conversion pour les applications */
	def evaluateConversion():Expression =
		{
			//on parse l'expression gauche pour voir si c'est une fonction
			this.exprLeft match
			{
				case Function(_,_) => 
					//l'expression gauche de l'application est une fonction, on tente donc un eventuel renommage des variables liees conflictuelles
					return  new Application(this.exprLeft.asInstanceOf[Function].applyConversion(this.exprRight),this.exprRight);
				case _ =>
					//on lance la conversion sur la partie gauche
					val newExprLeft:Expression = this.exprLeft.evaluateConversion();

					//si il y a eu modification on retourne la nouvelle application
					if (newExprLeft != this.exprLeft){
						return new Application(newExprLeft, this.exprRight)
					}

					//on lance la conversion sur la partie droite
					val newExprRight:Expression = this.exprRight.evaluateConversion();

					//si les 2 expressions de l'application sont restees identiques, on retourne l'objet lui meme (rien n'a change)
					if (newExprRight == this.exprRight){
						return this
					}

					//on retourne la nouvelle application
					return new Application(newExprLeft, newExprRight);
			}
		}

	/** beta-reduction pour les applications */
	override def alphaConversion(boundVar:Variable, expression:Expression, change:Boolean):Expression =
	{
			return new Application(this.exprLeft.alphaConversion(boundVar,expression,change),this.exprRight.alphaConversion(boundVar,expression,change))
	}	

	def evaluateReduction():Expression =
		{
			this.exprLeft match
			{
			case Function(_,_) =>
									//l'expression gauche de l'application est une fonction, on tente donc un eventuel renommage des variables liees conflictuelles
									return this.exprLeft.asInstanceOf[Function].applyReduction(this.exprRight)
			case _ =>
						//on lance la reduction sur la partie gauche
						val newExprLeft:Expression = this.exprLeft.evaluateReduction();

						//si il y a eu modification on retourne la nouvelle application
						if (newExprLeft != this.exprLeft){
							return new Application(newExprLeft, this.exprRight)
						}

						//on lance la reduction sur la partie droite
						val newExprRight:Expression = this.exprRight.evaluateReduction();

						//si les 2 expressions de l'application sont restees identiques, on retourne l'objet lui meme (rien n'a change)
						if (newExprRight == this.exprRight){
							return this
						}

						//on retourne la nouvelle application
						return new Application(newExprLeft, newExprRight);
						}
		}

	/** beta-reduction pour les applications */
	override def betaReduction(boundVar:Variable, expression:Expression):Expression =
	{
			return new Application(this.exprLeft.betaReduction(boundVar,expression),this.exprRight.betaReduction(boundVar,expression))
	}	

	/**affichage*/
	override def toString():String =
		{
			return "Application("+exprLeft.toString()+","+exprRight.toString()+")"
		}

	/**liste des variables libres de l'application*/
	override def freeVariables():List[Variable] =
		{
			return this.exprLeft.freeVariables().:::(this.exprRight.freeVariables());
		}

	override def allVariables():List[Variable] =
	{
			return this.exprLeft.allVariables().:::(this.exprRight.allVariables());
	}

}

/**----------------------------------------------------------------------------------------------*/

/** les fonctions de parsing de la chaine en entree*/

/** parser du type Expression*/
def parseExpression(str:String):Expression ={
		val n = str.length();
		var str1 = "";
		var str2 = "";
		var c = str.charAt(0);
		if (c=='('){
			str1 = this.searchExpression(str.substring(0,n));
			val n1 = str1.length();
			if (n1<(n-2)){
				str2 = this.searchExpression(str.substring(n1+2,n));
				return this.parseApplication(str1, str2)
			}
			else{
				return this.parseExpression(str1)
			}
		}
		if (c=='l'){
			return this.parseFunction(str.substring(1,n))
		}
		else{
			if (n==1){
				return this.parseVariable(c+"")
			}
			else{
				str1 = this.searchExpression(str.substring(0,1));
				str2 = this.searchExpression(str.substring(1,n));
				return this.parseApplication(str1, str2)			  
			}
		}
}

/** parser du type Variable*/
def parseVariable(ch:String):Variable ={
		return new Variable(ch)
}	

/** parser du type Function*/
def parseFunction(str:String):Function ={
		val n = str.length();
		var c:Char = str.charAt(0);
		if (str.charAt(1)!='.'){
			throw new ParserException("Fonction mal construite, '.' manquant");
		}
		return new Function(this.parseVariable(c+""), this.parseExpression(str.substring(2, n)))
}

/** parser du type Application*/
def parseApplication(str1:String, str2:String):Application ={
		return new Application(this.parseExpression(str1), this.parseExpression(str2))
}
/**----------------------------------------------------------------------------------------------*/

/** fonction de recherche de la fin d'une expression*/
def searchExpression(str:String):String ={
		val n = str.length();
		var i = 0;
		var nbOpen = 0;
		var c:Char = str.charAt(i);
		if (c.isLetter){
			return ""+c
		}
		while (i<n){
			c = str.charAt(i);
			if (c=='('){
				nbOpen+=1
			}
			if (c==')'){
				nbOpen-=1
			}
			if (nbOpen==0){
				return str.substring(1,i)
			}			
			i+=1;
		}
		throw new ParserException("Probleme de parentheses");
		return "";
}

def expressionToString(expr:Expression):String ={
  var str1:String = ""
  var str2:String = ""
  expr match{
    case Application(el,er) =>
      	str1 = el match
      	{
      	  case Function(_,_) => "(" + expressionToString(el) + ")";
      	  case Application(_,_) => "(" + expressionToString(el) + ")";
      	  case Variable(_) => "(" + expressionToString(el)		
      	}
      	str2 = er match
      	{
      	  case Function(_,_) => "(" + expressionToString(er) + ")"
      	  case Application(_,_) => "(" + expressionToString(er) + ")"
      	  case Variable(_) => expressionToString(er) + ")"      	  
      	}
      	return str1 + str2
    case Function(p,b) => return "l" + expressionToString(p) + "." + expressionToString(b)
    case Variable(v) => return v
  }
}

}