package infpp.oceanlife;

import java.awt.Graphics;
import java.io.Serializable;

/**
 * This is the Superclass for all Objects in our Ocean
 * 
 * @author Mirco Gassmann, Jan Hansen
 *
 */

@SuppressWarnings("serial")
public abstract class OceanObject implements Serializable {

	/**
	 * x-coordinate of our object
	 */
	protected int x;
	
	/**
	 * y-coordinate of our object
	 */
	protected int y;
	
	/**
	 * name of our object
	 */
	private String name;
	
	/**
	 * boolean to indicate if our object is selected
	 */
	protected boolean border;

	/**
	 * The Constructor to create an Object. We can save Code by using
	 * super(x,y,name) in the Subconstructors
	 * 
	 * @param x
	 *            the x-Position of the Object
	 * @param y
	 *            the y-Position of the Object
	 * @param name
	 *            the name of the Object
	 */

	public OceanObject(int x, int y, String name) {
		border = false;
		this.x = x;
		this.y = y;
		this.name = name;
	}

	/**
	 * Method to get the x-Position of the Object
	 * 
	 * @return returns the x-Position of the Object
	 */
	public int getX() {
		return x;
	}

	/**
	 * Method to set the x-Position of the Object
	 * 
	 * @param x
	 *            the x-Position we would like to set
	 */
	public void setX(int x) {
		this.x = x;
	}

	/**
	 * Method to get the y-Position of the Object
	 * 
	 * @return returns the y-Position of the Object
	 */
	public int getY() {
		return y;
	}

	/**
	 * Method to set the y-Position of the Object
	 * 
	 * @param y
	 *            the y-Position we would like to set
	 */
	public void setY(int y) {
		this.y = y;
	}

	/**
	 * Method to get the Name of the Object
	 * 
	 * @return returns the Name of the Object
	 */
	public String getName() {
		return name;
	}

	/**
	 * Method to set the Name of the Object
	 * 
	 * @param name
	 *            the Name we would like to set
	 */
	public void setName(String name) {
		this.name = name;
	}
	
	/**
     * setter method for the border flag
     * @param b  new flag
     */
    public void setBorder(boolean b){
        border = b;
    }
    
    /**
     * getter method for border flag
     * @return border flag
     */
    public boolean getBorder(){
        return border;
    }
    
	/**
	 * Abstract Method to move the Objects. Must be implemented by all Objects
	 * themselves. So we can implement different Movement for the Objects
	 * 
	 * @param width
	 *            the Width of the Ocean to check whether we are out of
	 *            Bounds(later)
	 * @param depth
	 *            the Depth of the Ocean to check whether we are out of
	 *            Bounds(later)
	 */

    /**
     * abstract move method for our objects
     * @param width The width of an ocean
     * @param depth The depth of an ocean
     */
	public abstract void move(int width, int depth);

	public abstract void draw(Graphics g);

	/**
	 * The toString-Method for the Objects. We need it to save Code in the
	 * Subclasses. Can be called by super.toString();
	 */
	@Override
	public String toString() {
		return this.name;
	}

	/**
	 * Method to return the Height of an object
	 * @return Height of an object
	 */
	public abstract int getHeight();

	/**
	 * Method to return the Width of an object
	 * @return Width of an object
	 */
	public abstract int getWidth();

}
