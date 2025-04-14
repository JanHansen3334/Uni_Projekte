package infpp.oceanlife;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.net.URL;
import java.util.LinkedList;

import javax.imageio.ImageIO;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

/**
 * an extension for JPanel which is able to draw an given Ocean
 * 
 * @author User
 */
@SuppressWarnings("serial")
public class OceanGraphic extends JPanel {

	/**
	 * the ocean we want to paint
	 */
	OceanInterface ocean;

	/**
	 * public static Images for all ocean Objects
	 */
	public static BufferedImage fishImage, plantImage, bubbleImage, stoneImage,
			sharkImage, sharkmirrorImage, fish2Image, fish3Image, fish4Image, fish5Image,
			fishmirrorImage, fish2mirrorImage, fish3mirrorImage, fish4mirrorImage, fish5mirrorImage;

	/**
	 * creates an OceanGraphic
	 * 
	 * @param oc ocean to handle
	 */
	OceanGraphic(OceanInterface oc) {
		ocean = oc;
		loadImages();
	}

	/**
	 * Do the Painting of the ocean when pain() is called
	 * 
	 * @param g
	 *            Graphics to paint on
	 */
	@Override
	public void paint(Graphics g) {
		// draw the background
		g.setColor(Color.decode("0x4f3ae8"));
		g.fillRect(0, 0, ocean.getWidth(), ocean.getDepth());
		// draw the sand
		g.setColor(Color.decode("0xfbd091"));
		g.fillRect(0, ocean.getDepth() - 20, ocean.getWidth(), 20);

		// draw all the oceanObjects
		LinkedList<OceanObject> objects = ocean.getOceanObjects();
		for (int i = 0; i < objects.size(); i++) {
			objects.get(i).draw(g);
		}
	}

	/**
	 * when creating an oceanGraphic load all the images
	 */
	private void loadImages() {
		// load the images
		try {
			URL url = OceanGraphic.class.getResource("media/fish.png");
			fishImage = ImageIO.read(url);

			url = OceanGraphic.class.getResource("media/plant.png");
			plantImage = ImageIO.read(url);

			url = OceanGraphic.class.getResource("media/bubble.png");
			bubbleImage = ImageIO.read(url);

			url = OceanGraphic.class.getResource("media/rock.png");
			stoneImage = ImageIO.read(url);

			url = OceanGraphic.class.getResource("media/shark.gif");
			sharkImage = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish2.png");
			fish2Image = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish3.png");
			fish3Image = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish4.png");
			fish4Image = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish5.png");
			fish5Image = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/sharkmirror.gif");
			sharkmirrorImage = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fishmirror.png");
			fishmirrorImage = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish2mirror.png");
			fish2mirrorImage = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish3mirror.png");
			fish3mirrorImage = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish4mirror.png");
			fish4mirrorImage = ImageIO.read(url);
			
			url = OceanGraphic.class.getResource("media/fish5mirror.png");
			fish5mirrorImage = ImageIO.read(url);

		} catch (Exception e) {
			// if there was an error inform the user and exit
			final JOptionPane optionPane = new JOptionPane();
			JOptionPane.showMessageDialog(optionPane,
					"Unable to load all of the pictures.", "ERROR",
					JOptionPane.ERROR_MESSAGE);
			System.exit(0);
		}
	}

	/**
	 * setter method to change the ocean to be drawn only call in synchronized
	 * context (later when you will use Threading)
	 * 
	 * @param _ocean the new ocean
	 */
	void setOcean(OceanInterface _ocean) {
		ocean = _ocean;
	}
}
