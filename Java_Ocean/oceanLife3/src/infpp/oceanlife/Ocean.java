package infpp.oceanlife;

import java.io.Serializable;
import java.util.LinkedList;

/**
 * An Ocean holds a number of OceanObjects
 * 
 * @author Mirco Gassmann, Jan Hansen
 *
 */

@SuppressWarnings("serial")
public class Ocean implements OceanInterface, Serializable {

	/**
	 * the Graphics of our Ocean
	 */
	private OceanGraphic oceanGraphic;
	
	/**
	 * width and depth of our ocean
	 */
	private int width, depth;
	
	/**
	 * List of Objects in our ocean
	 */
	LinkedList<OceanObject> oceanObjects;

	/**
	 * Constructor to set the Width,Depth and a List of Oceanobjects for an
	 * Ocean.
	 * 
	 * @param width
	 *            The Width of the Ocean
	 * @param depth
	 *            The Depth of the Ocean
	 * @param oceanObjects
	 *            The List with the OceanObjects
	 */

	public Ocean(int width, int depth, LinkedList<OceanObject> oceanObjects) {
		this.width = width;
		this.depth = depth;
		this.oceanGraphic = new OceanGraphic(this);
		this.oceanObjects = new LinkedList<OceanObject>();
		this.setOceanObjects(oceanObjects);
	}

	/**
	 * Method to set the Depth of an Ocean
	 * 
	 * @param depth
	 *            The Depth of an Ocean
	 */

	public void setDepth(int depth) {
		this.depth = depth;

	}

	/**
	 * Method to set the Width of an Ocean
	 * 
	 * @param width
	 *            The Width of an Ocean
	 */

	public void setWidth(int width) {
		this.width = width;

	}
	
	/** Method to set the graphics of our ocean.
	 * 
	 * @param g - The graphics we want our ocean to use.
	 */
	public void setGraphics(OceanGraphic g){
		this.oceanGraphic = g;
	}

	/**
	 * Getter for an Ocean
	 * 
	 * @return returns the depth as an int of an Ocean
	 */
	public int getDepth() {
		return depth;
	}

	/**
	 * Getter for an Ocean
	 * 
	 * @return returns the width as an int of an Ocean
	 */

	public int getWidth() {
		return width;
	}
	
	/** Method to get the graphics that this heaven uses.
	 * 
	 * @return - An instance of OceanGraphics, which has loaded all the images we use.
	 */
	public OceanGraphic getGraphics(){
		return this.oceanGraphic;
	}

	/**
	 * This Method iterates through the List and calls the Move-Method of the
	 * Objects. It works like this: Which Object is on Index i(get i) ? Call
	 * the specific Move-Method from the Class of this Object
	 */

	public void move() {
		// for-Loop ends when i >= Number of Objects in the
		// List(oceanObjects.size)
		for (int i = 0; i < oceanObjects.size(); i++) {
			// call the specific move-Method
			oceanObjects.get(i).move(getWidth(), getDepth());
		}
	}

	/**
	 * Method to print the State of the Oceans and the Objects inside the Ocean
	 * to the Terminal
	 */
	@Override
	public String toString() {
		String value;
		// Add the depth and width of the Ocean to the String
		value = "Ocean: width: " + width + " depth: " + depth + "\nObjects:\n";

		// Add the OceanObjects to the String
		for (int i = 0; i < oceanObjects.size(); i++) {
			// Call the toString-Method of the Object on Index i
			value = value + oceanObjects.get(i).toString() + "\n";
		}

		return value;
	}

	/**
	 * Get the List of the OceanObjects
	 * 
	 * @return The List of the OceanObjects
	 */
	@Override
	public LinkedList<OceanObject> getOceanObjects() {
		return oceanObjects;
	}

	/**
	 * Method to set the OceanObjects
	 * 
	 * @param oceanObjects
	 *            The List of the OceanObjects we would like to set
	 */
	@Override
	public void setOceanObjects(LinkedList<OceanObject> oceanObjects) {
		this.oceanObjects = oceanObjects;
	}
	/**
     * Add an OceanObject to the oceans list
     * @param object The object to be added
     */
	@Override
    public void addOceanObject(OceanObject object)  {
        oceanObjects.add(object);               
    }
	
	/**
	 * Delete an OceanObject from the oceans list
	 * @param i the index of the object to be deleted
	 */
	@Override
	public void deleteOceanObject(int i){
		oceanObjects.remove(i);
	}
}
