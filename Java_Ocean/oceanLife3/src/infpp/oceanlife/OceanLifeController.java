package infpp.oceanlife;

import java.util.LinkedList;
/**
 * The Class for our controller
 * @author Mirco Gassmann, Jan Hansen
 *
 */
public class OceanLifeController {

	/**
	 * Thread for painting
	 */

	private PaintThread paintThread;
	/**
	 * Thread for moving
	 */
	private MoveThread moveThread;

	/**
	 * Our ocean (also the model in our case)
	 */
	Ocean model;

	/**
	 * Our GUI ( also the view in our case)
	 */
	OceanGUI view;

	/**
	 * We are creating OceanObjects and add them to a List. We are moving the
	 * Objects by using the move-Method of the Ocean. We print the State
	 * directly after creating the Objects and again after moving the Objects
	 * one time by using the to-String-Method of the Oceanclass.
	 */

	public void start() {

		// Creating the Objects
		Fish fish = new Fish(54, 19, "Fish");
		Stone stone = new Stone(50, 50, "Stone");
		Plant plant = new Plant(1400, 750, "Plant");
		Bubble bubble = new Bubble(245, 650, "Bubble");
		Shark shark = new Shark(400, 400, "Shark");
		// Creating a List to manage the Objects in an easy way
		LinkedList<OceanObject> oceanObjects = new LinkedList<OceanObject>();
		// Adding the Objects to the List
		oceanObjects.add(fish);
		oceanObjects.add(stone);
		oceanObjects.add(plant);
		oceanObjects.add(bubble);
		oceanObjects.add(shark);
		// Creating an Ocean with the Width 2000, the depth 1000 and the List
		// with the Objects
		Ocean ocean = new Ocean(2000, 1000, oceanObjects);
		// Print the State after Creating
		System.out.println(ocean);
		// Move the Objects
		ocean.move();
		// Print the State after Moving the Objects
		System.out.println(ocean);
		Ocean ocean2 = new Ocean(750, 600, oceanObjects);
		model = ocean2;
		OceanGUI gui = new OceanGUI(this);
		gui.setVisible(true);
		gui.repaint();
		gui.pack();
		view = gui;
		// creating the threads and start them
		paintThread = new PaintThread(view, model, view.removeList);
		moveThread = new MoveThread(model, view.removeList);
		paintThread.start();
		moveThread.start();

	}
	/** Method to get the model that our controller currently uses.
	 * 
	 * @return - Returns the controlled model.
	 */
	public OceanInterface getModel(){
		return this.model;
	}
	
	/** Method to set the model that our controller currently uses.
	 * 
	 * @param hc - The model we want our controller to control.
	 */
	public void setModel(OceanInterface hc){
		this.model = (Ocean) hc;
	}
	
	/** Method to get our moveThread.
	 * 
	 * @return - Returns our MoveThread.
	 */
	public MoveThread getMoveThread(){
		return this.moveThread;
	}
	
	/** Method to set our moveThread to a new MoveThread.
	 * 
	 * @param mt - The new MoveThread we want our controller to use.
	 */
	public void setMoveThread(MoveThread mt){
		this.moveThread = mt;
	}
	
	/** Getter to get our PaintThread
	 * 
	 * @return - Returns our paintThread.
	 */
	public PaintThread getPaintThread(){
		return this.paintThread;
	}
	
	/** Method to set our PaintThread to a new PaintThread
	 * 
	 * @param pt - The new PaintThread we want our controller to use.
	 */
	public void setPaintThread(PaintThread pt){
		this.paintThread = pt;
	}

}
