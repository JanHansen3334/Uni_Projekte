package infpp.oceanlife;

import javax.swing.JList;

/**
 * Class for the Thread that does the Painting of our ocean
 * @author Mirco Gassmann, Jan Hansen
 *
 */
public class PaintThread extends Thread{

	/**
	 * the given gui to rapaint
	 */
	private OceanGUI gui;
	/**
	 * the ocean to synchronize
	 */
	private OceanInterface ocean;
	/**
	 * static int for framerate
	 */
	private static long maxFPS = 30;
	/**
	 * Jlist to synchronize
	 */
	private JList removeList;
	/**
	 * flag for terminating the run() method
	 */
	private boolean running;

	/**
	 * Create a PaintThread with an GUI to repaint and the lock objects
	 * 
	 * @param g
	 *            GUI to repaint
	 * @param _ocean
	 *            Ocean to synchronize
	 * @param list
	 *            List to synchronize
	 */
	PaintThread(OceanGUI g, OceanInterface _ocean, JList list) {
		removeList = list;
		gui = g;
		ocean = _ocean;
		running = true;
	}

	/**
	 * When a thread is started do the painting once per frame
	 */
	public void run() {
		while (true) {
			long startTime = System.currentTimeMillis();
			synchronized (ocean) {
				synchronized (removeList) {
					// reapint the gui synchronized
					gui.repaint();
				}
			}
			// if the terminate method was called quit the run() method
			// (terminate)
			if (!running) {
				return;
			}

			// Framelimiter
			long endTime = System.currentTimeMillis();
			long sleepTime = 1000 / maxFPS - (endTime - startTime);
			if (sleepTime > 0) {
				try {
					Thread.sleep(sleepTime);
				} catch (InterruptedException ex) {
				}
			}
		}
	}

	/**
	 * Terminate the thread after next loop
	 */
	void terminate() {
		running = false;
	}

	/**
	 * change the lock object ocean e.g. after loading a new ocean
	 * 
	 * @param _ocean The new Ocean
	 */
	void setOcean(OceanInterface _ocean) {
		synchronized (ocean) {
			synchronized (removeList) {
				ocean = _ocean;
			}
		}
	}
}
