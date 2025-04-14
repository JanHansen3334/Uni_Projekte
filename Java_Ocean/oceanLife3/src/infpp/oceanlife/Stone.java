package infpp.oceanlife;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

/**
 * The class for the Stone
 * 
 * @author Mirco Gassmann, Jan Hansen
 *
 */

@SuppressWarnings("serial")
public class Stone extends OceanObject {

	/**
     * static height of a Stone
     */
    public final static int HEIGHT=60;
    /**
     * static width of a Stone
     */
    public final static int WIDTH=80;
    
    /**
     * the counter of stones created
     */
    private static int counter = 1;
    
	/**
	 * Creating a Stone using the Constructor of the Superclass
	 * 
	 * @param x
	 *            The x-Position of the Stone
	 * @param y
	 *            The y-Position of the Stone
	 * @param name
	 *            The Name of the Stone
	 */
	public Stone(int x, int y, String name) {
		super(x, y, name + counter);
		counter++;
	}

	/**
     * Get the height of a Stone
     * @return Heigth of a Stone
     */
    @Override
    public int getHeight(){
        return Stone.HEIGHT;
    }
    
    /**
     * Get the width of a Stone
     * @return width of a Stone
     */
    @Override
    public int getWidth(){
        return Stone.WIDTH;
    }
    
	/**
	 * The move-Method for the Stone. If the stone is not set on the ground
	 * he will be set there.
	 */

	@Override
	public void move(int width, int depth) {
		if (this.getY() != depth - HEIGHT){
			this.setY(depth - HEIGHT);
		}
		if((this.getX() < 0) || (this.getX() > width)){
			this.setX(width - WIDTH);
			this.setY(depth - HEIGHT);
		} else {
			this.setX(this.getX() - 0);
			this.setY(this.getY() + 0);
		}
	}

	/**
	 * This method uses the toString-Method of the Superclass to create a String
	 * with the current state of the Stone.
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
		temp = OceanGraphic.stoneImage;
		g.drawImage(temp, this.getX(), this.getY(), null);
		//draw border if necessary
        if (border == true){
            g.setColor(Color.red);
            g.drawRect(this.getX(),this.getY(),getWidth() ,getHeight());
        }
	}

}
