package lambdaGraph;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import eval.*;

public class Gui extends JApplet implements ActionListener{
	// A Button to click
	Button buttonReduice;
	Button buttonBack;
	Button buttonDel;

	Label nameApplet;
	Label nameGraph1;
	Label nameGraph2;
	
	// A textField to get text input
	TextField formulaField;

	//Explication
	TextArea explicationField;
	TextArea exampleField;

	Lambda.Expression exp;

	JScrollPane treeScrollPaneLeft;
	JScrollPane treeScrollPaneRight;

	ArrayList<Lambda.Expression> listExp = new ArrayList<Lambda.Expression>(); 
	@Override
	public void init()
	{
		setLayout(null);
		setSize(1300,750);
		//Labels
		nameApplet=new Label("Visualisation de la réduction de lambda-termes");
		nameApplet.setFont(new Font("Verdana", java.awt.Font.BOLD, 20));
		nameApplet.setAlignment(WIDTH);
		nameGraph1=new Label("Précédent: ");
		nameGraph1.setAlignment(WIDTH);
		nameGraph2=new Label("Actuel: ");
		nameGraph2.setAlignment(WIDTH);

		//buttons
		buttonReduice = new Button("Reduire");
		buttonBack = new Button("Retour");
		buttonBack.setEnabled(false);
		buttonDel = new Button("Effacer");

		// text and length of the field
		formulaField = new TextField("Entrer une formule");
		explicationField = new TextArea("Manuel: \n"+
				"- Ecrire \"lx.ly\" au lieu de \"lxy\".\n"+
				"- Une application est toujours entre les parenthèses:\n"+
				"	Par exemple: (SK), ((SK)T)\n"+
				"- Les espaces dans les formules sont autorisés.\n"+
				"- Pour changer la formule, cliquez d'abord sur \"Effacer\""
				);
		exampleField = new TextArea("Test 1 (delta A):\n((lx.xx)(lx.ly.xy))\n\n"+
				"Test 2 (SKK):\n((lx.ly.lz.((xz)(yz)))(lx.ly.x))(lx.ly.x)\n\n"+
				"Test 3 (S(KS)K):\n(((lx.ly.lz.((xz)(yz)))(((lx.ly.x))(lx.ly.lz.xz(yz)))))(lx.ly.x)");

		// now we will specify the positions of the GUI components.
		nameApplet.setBounds(10, 10, 1300, 20);
		nameGraph1.setBounds(10, 550, 600, 20);
		nameGraph2.setBounds(650, 550, 600, 20);
		add(nameApplet);
		add(nameGraph1);
		add(nameGraph2);		

		buttonReduice.setBounds(20,620,100,30);
		buttonBack.setBounds(130,620,100,30);
		buttonDel.setBounds(240,620,100,30);
		add(buttonReduice);
		add(buttonBack);
		add(buttonDel);
		buttonReduice.addActionListener(this);
		buttonBack.addActionListener(this);
		buttonDel.addActionListener(this);
		
		formulaField.setBounds(20,580,400,20);
		add(formulaField);
		explicationField.setBounds(440,580,400,150);
		explicationField.setEditable(false);
		exampleField.setBounds(850,580,400,150);
		exampleField.setEditable(false);

		add(explicationField);
		add(exampleField);

		//Two JScrollPane which contains the Lambda-Calcul trees
		treeScrollPaneLeft= new JScrollPane(
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

		treeScrollPaneLeft.getViewport().setBackground(Color.cyan);
		treeScrollPaneLeft.setBounds(10, 50, 600, 500);
		treeScrollPaneLeft.setPreferredSize(new Dimension(800, 250));
		treeScrollPaneLeft.setViewportBorder(
                BorderFactory.createLineBorder(Color.black));
		add(treeScrollPaneLeft);

		treeScrollPaneRight = new JScrollPane(
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		treeScrollPaneRight.getViewport().setBackground(Color.cyan);
		treeScrollPaneRight.setBackground(Color.cyan);
		treeScrollPaneRight.setBounds(650, 50, 600, 500);
		treeScrollPaneRight.setPreferredSize(new Dimension(800, 250));
		treeScrollPaneRight.setViewportBorder(
                BorderFactory.createLineBorder(Color.black));
		add(treeScrollPaneRight);
	}
	@Override
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource() == buttonReduice) 
		{	
			if (exp==null){
				try{
					String formula=formulaField.getText();
					formula=formula.replaceAll(" ", "");
					exp=Lambda.parseExpression(formula);

					listExp.add(exp);
					BinaryTree b=new BinaryTree(exp);
					treeScrollPaneRight.add(b);
					treeScrollPaneRight.setViewportView(b);
					nameGraph2.setText("Actuel: "+formula);
					//buttonBack.setEnabled(true);
				}
				catch (Exception e){
					if (e instanceof ParserException){
						formulaField.setText(((ParserException) e).displayException()+
								": "+formulaField.getText());
					}
					else{
						formulaField.setText("Verifiez votre formule: "+
								": "+formulaField.getText());
					}
				}
			}
			else{
				exp=listExp.get(listExp.size()-1);
				Lambda.Expression expEvaluated =exp.evaluate();
				if (exp!=expEvaluated){
					BinaryTree b=new BinaryTree(exp);
					treeScrollPaneLeft.add(b);
					treeScrollPaneLeft.setViewportView( b);
					nameGraph1.setText("Précédent: "+Lambda.expressionToString(exp));

					exp=expEvaluated;
					listExp.add(exp);
					BinaryTree b1=new BinaryTree(exp);
					treeScrollPaneRight.add(b1);
					treeScrollPaneRight.setViewportView(b1);
					//buttonReduice.setEnabled(false);
					buttonBack.setEnabled(true);
					System.out.println(exp.toString()+"\n");

					nameGraph2.setText("Actuel: "+Lambda.expressionToString(exp));
				}
				else{
					buttonReduice.setEnabled(false);
				}
			}
		}

		else if (evt.getSource() == buttonBack) 
		{
			if (listExp.size()==2){
				treeScrollPaneLeft.setViewportView(null);

				exp=listExp.get(0);
				BinaryTree b1=new BinaryTree(exp);
				treeScrollPaneRight.add(b1);
				treeScrollPaneRight.setViewportView(b1);

				buttonBack.setEnabled(false);
				listExp.remove(listExp.size()-1);
				buttonReduice.setEnabled(true);
			}
			else{
				listExp.remove(listExp.size()-1);
				exp=listExp.get(listExp.size()-2);
				BinaryTree b=new BinaryTree(exp);
				treeScrollPaneLeft.add(b);
				treeScrollPaneLeft.setViewportView(b);

				exp=listExp.get(listExp.size()-1);
				BinaryTree b1=new BinaryTree(exp);
				treeScrollPaneRight.add(b1);
				treeScrollPaneRight.setViewportView(b1);

				buttonReduice.setEnabled(true);
			}
		}		
		else if (evt.getSource() == buttonDel) 
		{
			treeScrollPaneLeft.setViewportView(null);
			treeScrollPaneRight.setViewportView(null);
			formulaField.setText("");
			nameGraph1.setText("Précédent");
			nameGraph2.setText("Actuel");
			exp=null;
			listExp.clear();
			buttonBack.setEnabled(false);
			buttonReduice.setEnabled(true);
		}
	}
}
