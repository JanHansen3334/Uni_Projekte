package infpp.oceanlife;

import java.util.LinkedList;

/**
 * The Interface for our Ocean. The classes which implements this Interface has
 * to implement all Methods which are given here.
 * 
 * @author Mirco Gassmann, Jan Hansen
 *
 */

public interface OceanInterface {

	/**
	 * Get the width of an ocean
	 * 
	 * @return width the Width of the Ocean
	 */
	public int getWidth();

	/**
	 * Set the width of an ocean
	 * 
	 * @param width
	 *            The width we would like to set for the Ocean
	 */
	public void setWidth(int width);

	/**
	 * Get the depth of an ocean
	 * 
	 * @return depth the Width of the Ocean
	 */
	public int getDepth();

	/**
	 * Set the depth of an ocean
	 * 
	 * @param depth
	 *            The depth we would like to set for the Ocean
	 */
	public void setDepth(int depth);
	
	/** Method to get the graphics that this heaven uses.
	 * 
	 * @return - An instance of HeavenGraphics, which has loaded all the images we use.
	 */
	public OceanGraphic getGraphics();
	
	/** Method to set the graphics of our heaven.
	 * 
	 * @param g - The graphics we want our heaven to use.
	 */
	public void setGraphics(OceanGraphic g);

	/**
	 * Get the LinkedList of OceanObjects of an ocean
	 * @return the list of objects in the ocean
	 */
	public LinkedList<OceanObject> getOceanObjects();

	/**
	 * Set the linkedList of OceanObjects of an ocean
	 * 
	 * @param oceanObjects
	 *            a LinkedList of OceanObjects
	 */
	public void setOceanObjects(LinkedList<OceanObject> oceanObjects);

	/**
	 * Method to move all the Objects which are in the Ocean
	 */
	public void move();

	/**
	 * The Method to create a String with the State of the Ocean in the
	 * contained Objects
	 * 
	 */

	@Override
	public String toString();
	
	/**
     * Add an OceanObject to the oceans list
     * @param object The object to be added
     */

	public void addOceanObject(OceanObject object);
	
	/**
	 * Delete an OceanObect from the oceans list
	 * @param i to be deleted
	 */
	
	public void deleteOceanObject(int i);

}

