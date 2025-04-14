package infpp.oceanlife;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

/**
 * The class for the ocean object shark
 * @author Mirco Gassmann, Jan Hansen
 *
 */
@SuppressWarnings("serial")
public class Shark extends OceanObject {
	
	/**
     * Static height of a shark
     */
    public final static int HEIGHT= 80;
    
    /**
     * Static width of a shark
     */
    public final static int WIDTH= 160;
    
    /**
     * Direction of our fish ( 4 possible directions )
     */
    public int direction = 3;
    
    /**
     * counter for the sharks created
     */
    private static int counter = 1;

    
	
	/**
	 * Creating a Bubble using the Constructor of the Superclass
	 * 
	 * @param x
	 *            The x-Position of the Shark
	 * @param y
	 *            The y-Position of the Shark
	 * @param name
	 *            The Name of the Shark
	 */
	public Shark(int x, int y, String name) {
		super(x, y, name + counter);
		counter++;
	}
	
	/**
     * Get the height of a Shark
     * @return Height of a Shark
     */
    @Override
    public int getHeight(){
        return Shark.HEIGHT;
    }
    
    /**
     * Get the width of a Shark
     * @return Width of a Shark
     */
    @Override
    public int getWidth(){
        return Shark.WIDTH;
    }

	/**
	 * The move-Method for the Shark. If the shark encounters the borders of the ocean
	 * he changes his direction.
	 */
	
		@Override
		public void move(int width, int depth) {
			if ((this.getY() < 0)){
				this.direction = 1;
			}
			if((this.getX() < 0)){
				this.direction = 2;
			}
			if(this.getY() > depth - HEIGHT){
				this.direction = 0;
			}
			if(this.getX() > width - WIDTH){
				this.direction = 3;
			}
			if(direction == 0){
				this.setX(this.getX() - 2);
				this.setY(this.getY() - 4);
			}
			if(direction == 1){
				this.setX(this.getX() + 6);
				this.setY(this.getY() - 1);
			}
			if(direction == 2){
				this.setX(this.getX() + 1);
				this.setY(this.getY() - 3);
			}
			else{
				this.setX(this.getX() - 1);
				this.setY(this.getY() + 2);
			}
		}

	

	/**
	 * This method uses the toString-Method of the Superclass to create a String
	 * with the current state of the Shark.
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
		if(direction == 1 || direction == 2){
			temp = OceanGraphic.sharkmirrorImage;
		}else {
			temp = OceanGraphic.sharkImage;
		}
		g.drawImage(temp, this.getX(), this.getY(), null);
		//draw border if necessary
        if (border == true){
            g.setColor(Color.red);
            g.drawRect(this.getX(),this.getY(),getWidth() ,getHeight());
        }
		
	}

}
