package infpp.oceanlife;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

/**
 * The class for the Fish
 * 
 * @author Mirco Gassmann, Jan Hansen
 *
 */

@SuppressWarnings("serial")
public class Fish extends OceanObject  {
	
	/**
     * width of a Fish in pixel
     */
    public final static int WIDTH = 66;
    
    /**
     * height of a Fish in pixel
     */
    public final static int HEIGHT = 38;
    
    /**
     * direction of the fish ( for the move method )
     */
    public int direction = 2;
    
    /**
     * color of the fish ( 5 different colors )
     */
    public int color = (int) (Math.random()*100000)%5;
    
    /**
     * counter of the fishes created
     */
    private static int counter = 1;
        

	/**
	 * Creating a Fish using the Constructor from the Superclass
	 * 
	 * @param x
	 *            The x-Position of the Fish
	 * @param y
	 *            The y-Position of the Fish
	 * @param name
	 *            The Name of the Fish
	 */
	public Fish(int x, int y, String name) {
		super(x, y, name + counter);
		counter++;
	}
	
	/**
     * Get the height of a Fish
     * @return Heigth of a Fish
     */
    @Override
    public int getHeight(){
        return Fish.HEIGHT;
    }
    
    /**
     * Get the width of a Fish
     * @return width of a Fish
     */
    @Override
    public int getWidth(){
        return Fish.WIDTH;
    }

	/**
	 * The move-Method for the Fish. If it encounters a border, the fish
	 * will change its direction
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
			this.setX(this.getX() + 5);
			this.setY(this.getY() - 2);
		}
		if(direction == 1){
			this.setX(this.getX() - 4);
			this.setY(this.getY() + 5);
		}
		if(direction == 2){
			this.setX(this.getX() + 2);
			this.setY(this.getY() + 3);
		}
		else{
			this.setX(this.getX() - 2);
			this.setY(this.getY() - 3);
		}
	}

	/**
	 * This method uses the toString-Method of the Superclass to create a String
	 * with the current state of the Fish.
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
		if(this.color == 0){
			if(direction == 0 || direction == 2){
				temp = OceanGraphic.fishmirrorImage;
			}else{
				temp = OceanGraphic.fishImage;
			}
		}
		if(this.color == 1){
			if(direction == 0 || direction == 2){
				temp = OceanGraphic.fish2mirrorImage;
			}else{
				temp = OceanGraphic.fish2Image;
			}
		}
		if(this.color == 2){
			if(direction == 0 || direction == 2){
				temp = OceanGraphic.fish3mirrorImage;
			}else{
				temp = OceanGraphic.fish3Image;
			}
		}
		if(this.color == 3){
			if(direction == 0 || direction == 2){
				temp = OceanGraphic.fish4mirrorImage;
			}else{
				temp = OceanGraphic.fish4Image;
			}
		}
		if(this.color == 4){
			if(direction == 0 || direction == 2){
				temp = OceanGraphic.fish5mirrorImage;
			}else{
				temp = OceanGraphic.fish5Image;
			}
		}
		g.drawImage(temp, this.getX(), this.getY(), null);
		//draw border if necessary
        if (border == true){
            g.setColor(Color.red);
            g.drawRect(this.getX(),this.getY(),getWidth() ,getHeight());
        }
	}

}
