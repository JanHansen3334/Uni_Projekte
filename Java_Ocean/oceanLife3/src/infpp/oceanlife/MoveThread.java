package infpp.oceanlife;

import java.awt.Rectangle;
import java.util.LinkedList;

import javax.swing.JList;

/**
 * The Thread which is in control of calculating the movements in our ocean
 * also checks for collisions
 * @author Mirco Gassmann, Jan Hansen
 *
 */
public class MoveThread extends Thread {

	/**
	 * boolean to indicate if our Thread is running
	 */
	private boolean running;
	
	/**
	 * The frames-per-second
	 */
	private static long FPS = 30;

	/**
	 * The ocean to we control
	 */
	private OceanInterface oi;

	/**
	 * our removeList we want to update
	 */
	private JList removeList;

	MoveThread(OceanInterface _oi, JList _removeList) {
		removeList = _removeList;
		oi = _oi;
		running = true;
	}

	@Override
	public void run() {
		while (true) {

			long startTime = System.currentTimeMillis();
			synchronized (oi) {
				synchronized (removeList) {
					LinkedList<OceanObject> objects = oi.getOceanObjects();
					for (int i = 0; i < objects.size(); i++) {
						objects.get(i).move(oi.getWidth(), oi.getDepth());
					}
					// calc collisions and add Objects to deleteList
					LinkedList<OceanObject> deleteList = this
							.getDeleteList(objects);

					// delete Objects and update list if necassary
					if (deleteList.size() > 0) {
						// Update Ocean
						objects.removeAll(deleteList);
						// save selection
						OceanObject object = (OceanObject) removeList
								.getSelectedValue();
						// update list
						removeList.setListData(objects.toArray());

						// if the previous selected item is still in the list
						// set it selected again
						if (objects.contains(object)) {
							removeList.setSelectedValue(object, true);
						} else {
							removeList.clearSelection();
						}
					}

				}

			}
			// quit if terminate() was called
			if (!running) {
				return;
			}

			// Set the thread to sleep until desired FPS is reached
			long endTime = System.currentTimeMillis();
			long sleepTime = 1000 / FPS - (endTime - startTime);
			if (sleepTime > 0) {
				try {
					Thread.sleep(sleepTime);
				} catch (InterruptedException ex) {
					// ignore
				}

			}
		}
	}

	/**
	 * Does a Shark and a Fish collide?
	 * 
	 * @param shark
	 *            Shark to check
	 * @param fish
	 *            Fish to check
	 * @return boolean if fish and shark collide
	 */
	private boolean collide(OceanObject shark, OceanObject fish) {

		// set up two Rectangles
		Rectangle sharkRect = new Rectangle(shark.getX(), shark.getY(),
				shark.getWidth(), shark.getHeight());

		Rectangle fishRect = new Rectangle(fish.getX(), fish.getY(),
				fish.getWidth(), fish.getHeight());

		// Check for collision and return value
		return sharkRect.intersects(fishRect);
	}

	/**
	 * method to tell our thread to stop running at the end of our next loop
	 */
	void terminate() {
		running = false;
	}

	/**
	 * Calculate Objects to be deleted
	 * 
	 * @param objects
	 *            List of Objects to check for collisions
	 * @return List of Objects that should be deleted
	 */
	private LinkedList<OceanObject> getDeleteList(
			LinkedList<OceanObject> objects) {

		// List with objects to be deleted
		LinkedList<OceanObject> deleteList = new LinkedList<OceanObject>();

		// Search for sharks
		for (int i = 0; i < objects.size(); i++) {
			if (objects.get(i).getName().startsWith("Shark")) {

				// If found a shark chek all fish for collision
				for (int j = 0; j < objects.size(); j++) {
					if (objects.get(j).getName().startsWith("Fish")) {

						// if they collide add them to the deleteList
						if (this.collide(objects.get(i), objects.get(j))) {
							deleteList.add(objects.get(j));
						}
					}
				}
			}
		}
		// return the complete deleteList
		return deleteList;
	}

	/**
	 * Change the ocean to control
	 * 
	 * @param _ocean
	 *            new Ocean to control
	 */
	void setOcean(OceanInterface _ocean) {
		synchronized (oi) {
			synchronized (removeList) {
				oi = _ocean;
			}
		}
	}
}