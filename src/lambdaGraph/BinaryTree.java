package lambdaGraph;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.geom.*;

import javax.swing.*;

import scala.*;
import java.util.*;
import java.lang.Math;
import eval.*;
public class BinaryTree extends JComponent implements Scrollable,
MouseMotionListener{


	/**		
	 * 
	 */
	private int maxUnitIncrement = 1;
	private static final long serialVersionUID = 1L;
	public Lambda.Expression expression;
	private double height=20;
	private double horizon=40;
	int cmpHeight=0;
	int cmpWidthR=0;
	int cmpWidthL=0;
	int cmp=1;
	int cpt=0;
	int compteur=0;
	boolean first=true;
	double xInit;

	public BinaryTree(Lambda.Expression expression) {
		this.expression=expression;
		//horizon=(numNodes(expression)*(40+10)-700)/numNodes(expression);
		double proportion=(double)LeafRight(expression)*(40+10)/(double)((LeafRight(expression)*(40+10))+LeafLeft(expression)*(40+10));
		xInit=700*proportion;
		//Let the user scroll by dragging to outside the window.
		setAutoscrolls(true); //enable synthetic drag events
		addMouseMotionListener(this);
		//System.out.println("horizon=:"+horizon+"nb=: "+numNodes(expression)+"proportion="+proportion);
	}



	public int calculLeft(Lambda.Expression expression){
		if (expression instanceof Lambda.Application){
			cpt++;
			calculRight(((Lambda.Application)expression).exprLeft());
		}
		else if(expression instanceof Lambda.Function){
			calculRight(((Lambda.Function)expression).body());
		}
		return cpt;
	}

	public int calculRight(Lambda.Expression expression){
		if (expression instanceof Lambda.Application){
			cpt++;
			calculRight(((Lambda.Application)expression).exprRight());
		}
		else if(expression instanceof Lambda.Function){
			calculRight(((Lambda.Function)expression).body());
		}

		return cpt;
	}

	public void primaryRun(Lambda.Expression expression){
		if (expression instanceof Lambda.Application && first){
			first=false;
			Lambda.Application a=(Lambda.Application) expression;
			if (a.exprLeft() instanceof Lambda.Application){

				cmpWidthL++;
			}
			if (a.exprRight() instanceof Lambda.Application){
				cmpWidthR++;
			} 
			primaryRun(a.exprLeft());
			primaryRun(a.exprRight());
			cpt=0;
			System.out.println("Application Right= "+calculRight(a.exprLeft()));
			System.out.println("Application run");
		}
		else if(expression instanceof Lambda.Function){
			primaryRun(((Lambda.Function) expression).body());
			System.out.println("Function run");
		}
		else if(expression instanceof Lambda.Variable){
			System.out.println("Variable run");
			System.out.println("Finish: "+cmpWidthR);
			System.out.println("Finish: "+cmpWidthL);

		}
		else{
			System.out.println("Error");
		}
	}

	public int numNodes(Lambda.Expression e){

		if (e instanceof Lambda.Variable) return 0;
		else if (e instanceof Lambda.Function) return numNodes(((Lambda.Function) e).body());		
		else return (1 + numNodes(((Lambda.Application) e).exprLeft()) + 
				numNodes(((Lambda.Application) e).exprRight()));

	}


	public int height(Lambda.Expression e){

		if (e instanceof Lambda.Variable) return 1;
		else if (e instanceof Lambda.Function){
			return height(((Lambda.Function) e).body())+1;
		}
		else {
			int heightLeft = height(((Lambda.Application) e).exprLeft());
			int heightRight = height(((Lambda.Application) e).exprRight());
			if( heightLeft > heightRight)
				return heightLeft +1;
			else
				return heightRight +1;
		}
	}

	public int LeafRight(Lambda.Expression e){

		if (e instanceof Lambda.Variable) return 1;
		else if (e instanceof Lambda.Function){
			return Leaf(((Lambda.Function) e).body());
		}
		else {
			return 1+Leaf(((Lambda.Application) e).exprRight());
		}
	}

	public int LeafLeft(Lambda.Expression e){

		if (e instanceof Lambda.Variable) return 1;
		else if (e instanceof Lambda.Function){
			return Leaf(((Lambda.Function) e).body());
		}
		else {
			return 1+Leaf(((Lambda.Application) e).exprLeft());
		}
	}

	public int Leaf(Lambda.Expression e){
		if (e instanceof Lambda.Variable) return 1;
		else if (e instanceof Lambda.Function){
			return Leaf(((Lambda.Function) e).body());
		}
		else {
			return 1+Leaf(((Lambda.Application) e).exprLeft())+Leaf(((Lambda.Application) e).exprRight());
		}
	}

	public int maxLeft(Lambda.Expression e){
		if (e instanceof Lambda.Application){
			int left=maxLeft(((Lambda.Application) e).exprLeft())+1;
			return left;
		}
		else if (e instanceof Lambda.Function){
			return maxLeft(((Lambda.Function) e).body());
		}
		else{
			return 0;
		}
	}


	public int maxRight(Lambda.Expression e){
		if (e instanceof Lambda.Application){
			int right=maxRight(((Lambda.Application) e).exprRight())+1;
			return right;//+left;
		}
		else if (e instanceof Lambda.Function){
			return maxRight(((Lambda.Function) e).body());
		}
		else{
			return 0;
		}
	}

	public void parseExpression(Graphics g, Lambda.Expression expression, Point2D point){
		if (expression instanceof Lambda.Application){			
			Lambda.Application a=(Lambda.Application) expression;
			cmp++;

			//2 points
			double x=point.getX();
			double y=point.getY();
			Point2D pointLeft=new Point2D.Double(x-(horizon+5)*(maxRight(a.exprLeft())+1),y+height);
			Point2D pointRight=new Point2D.Double(x+(horizon+5)*(maxLeft(a.exprRight())+1),y+height);

			//Point2D pointLeft=new Point2D.Double(x-(horizon+10)*(LeafRight(a.exprLeft())),y+height);
			//Point2D pointRight=new Point2D.Double(x+(horizon+10)*(LeafLeft(a.exprRight())),y+height);

			//System.out.println(" "+LeafRight(a.exprLeft())+" "+LeafLeft(a.exprRight())+" ");
			drawApplication(point, pointLeft, pointRight, g);
			parseExpression(g,((Lambda.Application) expression).exprLeft(), newPoint(pointLeft));
			parseExpression(g,((Lambda.Application) expression).exprRight(), newPoint(pointRight));
			//System.out.println("Application");
		}
		else if(expression instanceof Lambda.Variable){
			String s= ((Lambda.Variable) expression).value();
			drawVariable(point,g,s,false);
			//System.out.println("Variable");
		}
		else if(expression instanceof Lambda.Function){
			drawVariable(point,g,"l"+((Lambda.Function) expression).parameter().value(),true);
			drawFunction(point, g);
			Point2D pointNext=new Point2D.Double(point.getX(),point.getY()+height+40);
			parseExpression(g,((Lambda.Function) expression).body(),pointNext);
			//System.out.println("Function");

		}
		else{
			System.out.println("None");
		}
	}

	public Point2D pointLeft(Point2D point){
		double x=point.getX();
		double y=point.getY();
		return new Point2D.Double(x-horizon-20*cpt,y+height);
	}

	public Point2D pointRight(Point2D point){
		double x=point.getX();
		double y=point.getY();
		return new Point2D.Double(x+horizon,y+height);
	}

	public Point2D newPointLeft(Point2D point){
		double x=point.getX();
		double y=point.getY();
		return new Point2D.Double(x-20,y+20);
	}

	public Point2D newPointRight(Point2D point){
		double x=point.getX();
		double y=point.getY();
		return new Point2D.Double(x+20,y+20);
	}

	public Point2D newPoint(Point2D point){
		double x=point.getX();
		double y=point.getY();
		return new Point2D.Double(x,y+20);
	}

	public void drawApplication(Point2D point, Point2D pointLeft, Point2D pointRight, Graphics g){
		double x=point.getX();
		double y=point.getY();

		Graphics2D g2D= (Graphics2D) g.create();
		String s="^";
		FontMetrics metrics = g2D.getFontMetrics();
		int adv = metrics.stringWidth(s);
		int hgt = metrics.getHeight();
		g2D.setColor(Color.green);
		g2D.drawString(s, (float)x-adv/2, (float)y);
		Line2D lineLeft=new Line2D.Double(point,pointLeft);
		Line2D lineRight=new Line2D.Double(point,pointRight);
		g2D.setColor(Color.black);
		g2D.draw(lineLeft);
		g2D.draw(lineRight);

	}

	public void drawFunction(Point2D point, Graphics g){
		Graphics2D g2D= (Graphics2D) g.create();
		FontMetrics metrics = g2D.getFontMetrics();
		int adv = metrics.stringWidth("lx");
		int hgt = metrics.getHeight();	
		double x=point.getX();
		double y=point.getY()+hgt;
		Line2D line = new Line2D.Double(x,y,x,y+height);
		g2D.draw(line);
	}		

	public void drawVariable(Point2D point, Graphics g, String v, boolean isFunction){
		double x=point.getX();
		double y=point.getY();
		Graphics2D g2D= (Graphics2D) g.create();
		FontMetrics metrics = g2D.getFontMetrics();
		int adv = metrics.stringWidth(v);
		if (isFunction){
			g2D.setColor(Color.red);
		}
		else{
			g2D.setColor(Color.blue);
		}
		g2D.drawString(v, (float)x-adv/2, (float)y);

	}

	@Override
	public void paintComponent(Graphics g){
		Point2D tem=new Point2D.Double(height(expression)*60,40);
		//Point2D tem=new Point2D.Double(xInit,40);
		parseExpression(g,expression,tem);

	}


	public Dimension getPreferredSize() {
		return new Dimension(900, 800);
	}

	@Override
	public Dimension getPreferredScrollableViewportSize() {
		return getPreferredSize();
	}

	@Override
	public int getScrollableBlockIncrement(Rectangle visibleRect,
			int orientation,
			int direction) {
		if (orientation == SwingConstants.HORIZONTAL) {
			return visibleRect.width - maxUnitIncrement;
		} else {
			return visibleRect.height - maxUnitIncrement;
		}
	}
	
	public void setMaxUnitIncrement(int pixels) {
		maxUnitIncrement = pixels;
	}

	@Override
	public boolean getScrollableTracksViewportHeight() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean getScrollableTracksViewportWidth() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public int getScrollableUnitIncrement(Rectangle visibleRect,
											int orientation,
											int direction){
		//Get the current position.
		int currentPosition = 0;
		if (orientation == SwingConstants.HORIZONTAL) {
			currentPosition = visibleRect.x;
		} else {
			currentPosition = visibleRect.y;
		}

		//Return the number of pixels between currentPosition
		//and the nearest tick mark in the indicated direction.
		if (direction < 0) {
			int newPosition = currentPosition -
					(currentPosition / maxUnitIncrement)
					* maxUnitIncrement;
			return (newPosition == 0) ? maxUnitIncrement : newPosition;
		} else {
			return ((currentPosition / maxUnitIncrement) + 1)
					* maxUnitIncrement
					- currentPosition;
		}
	}



	@Override
	public void mouseDragged(MouseEvent e) {

	}



	@Override
	public void mouseMoved(MouseEvent e) {
		Rectangle r = new Rectangle(e.getX(), e.getY(), 1, 1);
		scrollRectToVisible(r);		
	}
}
