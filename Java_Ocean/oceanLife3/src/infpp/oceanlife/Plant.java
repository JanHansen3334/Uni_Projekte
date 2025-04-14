package infpp.oceanlife;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

/**
 * The Class for our Plant.
 * 
 * @author Mirco Gassmann, Jan Hansen
 *
 */

@SuppressWarnings("serial")
public class Plant extends OceanObject {

	/**
     * Static height of a Plant
     */
    public final static int HEIGHT= 93;
    
    /**
     * Static width of a Plant
     */
    public final static int WIDTH=65;
    
    /**
     * Counter for the plants created starting at 1
     */
    private static int counter = 1;
    
	/**
	 * Creating a Plant using the Constructor of the Superclass
	 * 
	 * @param x
	 *            The x-Position of the Plant
	 * @param y
	 *            The y-Position of the Plant
	 * @param name
	 *            The name of the Plant
	 */
	public Plant(int x, int y, String name) {
		super(x, y, name + counter);
		counter++;
	}

	/**
     * Get the height of a Plant
     * @return Height of a Plant
     */
    @Override
    public int getHeight(){
        return Plant.HEIGHT;
    }
    
    /**
     * Get the width of a Plant
     * @return Width of a Plant
     */
    @Override
    public int getWidth(){
        return Plant.WIDTH;
    }
    
	/**
	 * The move-Method for the Plant. If the plant isnt set on the ground it will be set there
	 * otherwise it will not move
	 */

	@Override
	public void move(int width, int depth) {
		if (this.getY() != depth - HEIGHT){
			this.setY(depth - HEIGHT);
		}
		if((this.getX() < 0) || (this.getX() > width)){
			this.setX(width - WIDTH);
			this.setY(depth - HEIGHT);
		}
		if (this.getY() == 0){
			this.setX(this.getX() + 0);
			this.setY(this.getY() - 0);
		}
	}

	/**
	 * This method uses the toString-Method of the Superclass to create a String
	 * with the current state of the Plant
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
		temp = OceanGraphic.plantImage;
		g.drawImage(temp, this.getX(), this.getY(), null);	
		//draw border if necessary
        if (border == true){
            g.setColor(Color.red);
            g.drawRect(this.getX(),this.getY(),getWidth() ,getHeight());
        }
	}

}
