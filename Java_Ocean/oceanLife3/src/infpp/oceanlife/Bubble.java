package infpp.oceanlife;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

/**
 * The Class for the Bubble
 * 
 * @author Mirco Gassmann, Jan Hansen
 *
 */

@SuppressWarnings("serial")
public class Bubble extends OceanObject  {

	/**
     * static height of a Bubble
     */
    private final static int HEIGHT= 23;
    /**
     * static width of a Bubble
     */
    private final static int WIDTH= 23;
    
    /**
     * counter for the bubbles created, starting at 1
     */
    private static int counter = 1;
    
	/**
	 * Creating a Bubble using the Constructor of the Superclass
	 * 
	 * @param x
	 *            The x-Position of the Bubble
	 * @param y
	 *            The y-Position of the Bubble
	 * @param name
	 *            The Name of the Bubble
	 */
	public Bubble(int x, int y, String name) {
		super(x, y, name + counter);
		counter++;
	}

	/**
     * Get the height of a Plant
     * @return Heigth of a Plant
     */
    @Override
    public int getHeight(){
        return Bubble.HEIGHT;
    }
    
    /**
     * Get the width of a Plant
     * @return width of a Plant
     */
    @Override
    public int getWidth(){
        return Bubble.WIDTH;
    }
    
	/**
	 * The move-Method for the Bubble. If the bubble reaches the borders of the ocean
	 * it will appear on the other side of the ocean.
	 */
	@Override
	public void move(int width, int depth) {
		if ((this.getY() < 0) || (this.getY() > depth)){
			this.setY(depth - HEIGHT);
		}
		if((this.getX() < 0) || (this.getX() > width)){
			this.setX(width - WIDTH);
		}
		else {
			this.setX(this.getX() - 1);
			this.setY(this.getY() - 1);
		}

	}

	/**
	 * This method uses the toString-Method of the Superclass to create a String
	 * with the current state of the Bubble.
	 */
	@Override
	public String toString() {
		return super.toString();
	}

	/**
	 * The Method to repaint the GUI. We got the Image from the OceanGraphic-Class
	 * and then call the drawImage()-Function to draw the Image with the actual Position
	 * @param g the Graphic to be drawn
	 */
	@Override
	public void draw(Graphics g) {
		BufferedImage temp = null;
		temp = OceanGraphic.bubbleImage;
		g.drawImage(temp, this.getX(), this.getY(), null);
		//draw border if necessary
        if (border == true){
            g.setColor(Color.red);
            g.drawRect(this.getX(),this.getY(),getWidth() ,getHeight());
        }
		
	}

}
