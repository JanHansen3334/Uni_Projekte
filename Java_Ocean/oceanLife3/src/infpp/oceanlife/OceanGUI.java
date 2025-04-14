package infpp.oceanlife;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.LinkedList;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * The GUI for an OceanLife with all the Components. Starts two threads: one for
 * painting and one for the movements. Handles all the button and other GUIObject
 * events with nested classes.
 * 
 * @author Mirco Gassmann, Jan Hansen
 */
@SuppressWarnings("serial")
public class OceanGUI extends JFrame {
	
	/**
	 * Our controller
	 */
	private OceanLifeController controller;

	/**
	 * OceanGraphic object which contains the oceans drawing
	 */
	private OceanGraphic oceanGraphic;

	/**
	 * addButton adds objects to the ocean
	 */
	private JButton addButton;
	/**
	 * removeButton will remove selected objects from the ocean
	 */
	private JButton removeButton;
	/**
	 * clearButton clears all object in the removeList and the ocean
	 */
	private JButton clearButton;
	/**
	 * deselctButton resets the selection
	 */
	private JButton deselectButton;
	/**
	 * helpButton creates an informative pop-up
	 */
	private JButton helpButton;
	/**
	 * saveButton saves the ocean to a chosen file
	 */
	private JButton saveButton;
	/**
	 * loadButton tries to load an ocean from a selected file
	 */
	private JButton loadButton;
	/**
	 * quitButton closes GUI and exits the whole program
	 */
	private JButton quitButton;
	/**
	 * startButton runs the ocean again
	 */
	private JButton startButton;
	/**
	 * stopButton stops the ocean
	 */
	private JButton stopButton;
	/**
	 * stepButton only let the ocean to one more step
	 */
	private JButton stepButton;

	/**
	 * List which will display current objects
	 */
	@SuppressWarnings("rawtypes")
	public JList removeList;
	/**
	 * Scroll-able panel for the list
	 */
	private JScrollPane removeScrollPane;

	/**
	 * let the user choose which kind of object to be added
	 */
	@SuppressWarnings("rawtypes")
	private JComboBox addBox;

	/**
	 * x-position for adding objects
	 */
	private JTextField xInput;
	/**
	 * y-position for adding objects
	 */
	private JTextField yInput;

	/**
	 * xSlider and xInput will always have same value alternative way for input
	 */
	private JSlider xSlider;
	/**
	 * ySlider and yInput will always have same value alternative way for input
	 */
	private JSlider ySlider;

	/**
	 * labels for input objects
	 */
	private JLabel xLabel, yLabel, xLabel2, yLabel2;

	/**
	 * Create a OceanGUI with a specific OceanController to handle
	 * 
	 * @param oc
	 *            OceanController implementing oceanController to handle
	 */
	public OceanGUI(OceanLifeController oc) {

		// call the constructor for JFrames
		super("OceanLife");

		// set the ocean
		this.controller = oc;

		this.oceanGraphic = controller.getModel().getGraphics();
		// Initialize all the Components
		this.onInit();

		// setup the Layout
		this.setupLayout();

	}

	/**
	 * Initialize all the Components
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private void onInit() {
		// Listener for clicked buttons
		MouseListenerEx mouseListener = new MouseListenerEx(this);
		TextListenerEx textListener = new TextListenerEx();
		FocusListenerEx focusListener = new FocusListenerEx();
		SliderListener slideListener = new SliderListener();
		ListSelectionListenerEx listSelectionListener = new ListSelectionListenerEx();
		this.oceanGraphic.addMouseListener(mouseListener);

		// Initialize all the Button
		addButton = new JButton("Add");
		addButton.addMouseListener(mouseListener);
		removeButton = new JButton("Remove");
		removeButton.addMouseListener(mouseListener);
		clearButton = new JButton("Clear");
		clearButton.addMouseListener(mouseListener);
		deselectButton = new JButton("Deselect");
		deselectButton.addMouseListener(mouseListener);
		saveButton = new JButton("Save");
		saveButton.addMouseListener(mouseListener);
		helpButton = new JButton("Help");
		helpButton.addMouseListener(mouseListener);
		loadButton = new JButton("Load");
		loadButton.addMouseListener(mouseListener);
		quitButton = new JButton("Quit");
		quitButton.addMouseListener(mouseListener);
		startButton = new JButton("Start");
		startButton.addMouseListener(mouseListener);
		stopButton = new JButton("Stop");
		stopButton.addMouseListener(mouseListener);
		stepButton = new JButton("Step");
		stepButton.addMouseListener(mouseListener);

		// set up the list with removeable objects
		removeList = new JList(new DefaultListModel());
		// No need for synchro yet when setting the oceanObjects
		removeList.setListData(controller.getModel().getOceanObjects().toArray());
		removeList.setLayoutOrientation(JList.VERTICAL);
		removeList.setVisibleRowCount(JList.VERTICAL);
		removeList.addListSelectionListener(listSelectionListener);
		// add the List to the ScrollPane

		removeScrollPane = new JScrollPane(removeList);

		// create ComboBox to choose from
		addBox = new JComboBox(new String[] { "Fish", "Stone", "Plant",
				"Bubble", "Shark" });

		// set up input fields
		xInput = new JTextField("0");
		xInput.addActionListener(textListener);
		xInput.addFocusListener(focusListener);
		yInput = new JTextField("0");
		yInput.addActionListener(textListener);
		yInput.addFocusListener(focusListener);

		// set up sliders
		xSlider = new JSlider(JSlider.HORIZONTAL, 0, controller.getModel().getWidth(), 0);
		xSlider.addChangeListener(slideListener);

		ySlider = new JSlider(JSlider.HORIZONTAL, 0, controller.getModel().getDepth(), 0);
		ySlider.addChangeListener(slideListener);

		// initialize text labels
		xLabel = new JLabel("X:");
		yLabel = new JLabel("Y:");
		xLabel2 = new JLabel("X:");
		yLabel2 = new JLabel("Y:");
	}

	/**
	 * Add Components to Panels and set up the Layout for the GUI frame
	 */
	private void setupLayout() {
		setLayout(new GridBagLayout());
		// Constraints defaults to add all the objects
		GridBagConstraints c = new GridBagConstraints();
		c.weightx = 0.5;
		c.weighty = 0.5;
		c.insets = new Insets(4, 4, 4, 4);

		// the main panel on the right
		JPanel mainPanel = new JPanel(new GridBagLayout());

		// the panel with elements to add objects
		JPanel addPanel = new JPanel(new GridBagLayout());

		// add the box to choose
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.fill = GridBagConstraints.HORIZONTAL;
		addPanel.add(addBox, c);

		// add the Label "X:"
		c.gridx = 0;
		c.gridy = 1;
		c.gridwidth = 1;
		c.fill = GridBagConstraints.NONE;
		addPanel.add(xLabel, c);

		// add the input field for x
		c.gridx = 1;
		c.gridy = 1;
		c.ipadx = 40;
		addPanel.add(xInput, c);

		// add the Label "Y:"
		c.gridx = 2;
		c.gridy = 1;
		c.ipadx = 0;
		addPanel.add(yLabel, c);

		// add the input field for y
		c.gridx = 3;
		c.gridy = 1;
		c.ipadx = 40;
		addPanel.add(yInput, c);

		// add the add button
		c.gridx = 4;
		c.gridy = 1;
		c.ipadx = 0;
		addPanel.add(addButton, c);

		// add xLabel2
		c.gridx = 0;
		c.gridy = 2;
		addPanel.add(xLabel2, c);

		// add the xSlider
		c.gridx = 1;
		c.gridy = 2;
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.fill = GridBagConstraints.BOTH;
		addPanel.add(xSlider, c);

		// add yLabel2
		c.gridx = 0;
		c.gridy = 3;
		c.gridwidth = 1;
		c.fill = GridBagConstraints.NONE;
		addPanel.add(yLabel2, c);

		// add the ySlider
		c.gridx = 1;
		c.gridy = 3;
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.fill = GridBagConstraints.BOTH;
		addPanel.add(ySlider, c);

		// add the add Panel to the right main panel
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(0, 0, 0, 0);
		c.gridwidth = 1;
		mainPanel.add(addPanel, c);

		// the panel for elements to remove objects
		JPanel removePanel = new JPanel(new GridBagLayout());

		// add the List with removeable objects to the remove panel
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 75;
		c.gridheight = GridBagConstraints.REMAINDER;
		c.fill = GridBagConstraints.VERTICAL;
		c.anchor = GridBagConstraints.FIRST_LINE_START;
		c.insets = new Insets(4, 4, 4, 4);
		removePanel.add(removeScrollPane, c);

		// add the remove button to the remove panel
		c.gridx = 1;
		c.gridy = 0;
		c.ipady = 0;
		c.ipadx = 0;
		c.gridheight = 1;
		c.weighty = 0.0;
		c.fill = GridBagConstraints.NONE;
		removePanel.add(removeButton, c);

		// add the clear button to the remove panel
		c.gridx = 1;
		c.gridy = 1;
		removePanel.add(clearButton, c);

		// add the deselect button to the remove panel
		c.gridx = 1;
		c.gridy = 2;
		c.weighty = 1.0;
		removePanel.add(deselectButton, c);

		// add the remove Panel to the right main panel
		c.gridx = 0;
		c.gridy = 1;
		c.weightx = 0.5;
		c.fill = GridBagConstraints.BOTH;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = new Insets(0, 0, 0, 0);
		mainPanel.add(removePanel, c);

		// the panel with the buttons
		JPanel buttonPanel = new JPanel(new GridBagLayout());

		// add the help button
		c.gridx = 1;
		c.gridy = 0;
		c.weightx = 1.0;
		c.fill = GridBagConstraints.NONE;
		c.insets = new Insets(4, 4, 4, 4);
		buttonPanel.add(helpButton, c);

		// add the start button
		c.gridx = 0;
		c.gridy = 1;
		c.weightx = 0.0;
		buttonPanel.add(startButton, c);

		// add the stop button
		c.gridx = 1;
		c.gridy = 1;
		buttonPanel.add(stopButton, c);

		// add the step button
		c.gridx = 2;
		c.gridy = 1;
		buttonPanel.add(stepButton, c);

		// add the save button
		c.gridx = 0;
		c.gridy = 2;
		buttonPanel.add(saveButton, c);

		// add the load button
		c.gridx = 1;
		c.gridy = 2;
		buttonPanel.add(loadButton, c);

		// add the quit button
		c.gridx = 2;
		c.gridy = 2;
		buttonPanel.add(quitButton, c);

		// add the button Panel to the right main panel
		c.gridx = 0;
		c.gridy = 2;
		c.weightx = 0.5;
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(0, 0, 0, 0);
		mainPanel.add(buttonPanel, c);

		// add the Graphics to the oceanPanel
		JPanel oceanPanel = new JPanel(new GridBagLayout());

		// add the oceanGraphic to the big oceanPanel
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(0, 0, 0, 0);
		oceanPanel.add(oceanGraphic, c);

		// add the ocean Panel containing only the Graphics
		c.gridx = 0;
		c.gridy = 0;
		c.ipady = controller.getModel().getDepth() - 10;
		c.ipadx = controller.getModel().getWidth() - 10;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.fill = GridBagConstraints.NONE;
		add(oceanPanel, c);

		// add the main right Panel containing all the sub panels
		c.gridx = 1;
		c.gridy = 0;
		c.ipady = 0;
		c.ipadx = 0;
		c.weightx = 0.0;
		c.weighty = 0.0;
		c.fill = GridBagConstraints.BOTH;
		add(mainPanel, c);
	}

	/**
     * resets the flag for borders at every object in the current ocean
     */
    private void resetBorders(){
        LinkedList<OceanObject> objects = controller.getModel().getOceanObjects();
        for(int i=0;i<objects.size();i++){
            objects.get(i).setBorder(false);
        }
    }
    
	/**
	 * Nested Class for all button events in the OceanGUI Mainly to handle
	 * clicked buttons.
	 */
	class MouseListenerEx extends MouseAdapter {

		/**
		 * GUI that uses this listener
		 */
		private OceanGUI gui;

		/**
		 * Create a MouseListener with a GUI that uses this Listener
		 * 
		 * @param _gui The GUI
		 */
		public MouseListenerEx(OceanGUI _gui) {
			gui = _gui;
		}

		/**
		 * Handle mouse events theses might change objects in ocean and
		 * removeList, but will use synchronized blocks to do so
		 * 
		 * @param e The MouseEvent
		 */
		@Override
		public void mouseClicked(MouseEvent e) {
			Object source = e.getSource();
			// Quit Button
			if (source == quitButton) {
				this.handleQuitButton();
			}
			// Add Button
			else if (source == addButton) {
				this.handleAddButton();
			}
			// Step Button
			else if (source == stepButton) {
				this.handleStepButton();
			}
			// Remove Button
			else if (source == removeButton) {
				this.handleRemoveButton();
			}
			// Clear Button
			else if (source == clearButton) {
				this.handleClearButton();
			}
			// Load Button
			else if (source == loadButton) {
				this.handleLoadButton();
			}
			// Save Button
			else if (source == saveButton) {
				this.handleSaveButton();
			// Help Button
			}
			else if(source == helpButton){
                this.handleHelpButton();
			}
			// Deselect Button
			else if (source == deselectButton) {
				this.handleDeselectButton();
			}
			//Start Button
			else if (source == startButton){
				//when a thread was terminated by pushing the Stop Button we have
				//to create a new one.
				if(controller.getMoveThread().getState()==Thread.State.TERMINATED){
                    controller.setMoveThread(new MoveThread(controller.getModel(), removeList));
                    controller.getMoveThread().start();
                }
			}
			else if (source == stopButton){
				controller.getMoveThread().terminate();
			}

		}

		/**
		 * Things to do when the saveButton was pressed
		 */
		private void handleSaveButton() {
			//Create a FileChooser
			final JFileChooser chooser = new JFileChooser();
			
			//Set the current Path
			chooser.setCurrentDirectory(new File("."));
			
			//Open Save Dialog
			int selection = chooser.showSaveDialog(gui);
			
			//check whether Save Button is checked in the Dialog
			if (selection == JFileChooser.APPROVE_OPTION)
			try {
				//make sure that file ends it .chag
				File selectedFile = chooser.getSelectedFile();
				if (!selectedFile.getAbsolutePath().endsWith(".chag")){
					selectedFile = new File(selectedFile.getAbsolutePath()+".chag");
				}
				
				//open the File Stream
				FileOutputStream os = new FileOutputStream(selectedFile);
				
				//open the Object Stream
				
				ObjectOutputStream oos = new ObjectOutputStream(os);
				
				//Write the Ocean into File
				oos.writeObject(controller.getModel());
				
				//Close Object Stream
				oos.close();
				
				//inform the user about success
				final JOptionPane jp = new JOptionPane();
				JOptionPane.showMessageDialog(jp, "Successfully saved the Ocean" , "Success", JOptionPane.INFORMATION_MESSAGE);
			}catch(Exception e){
				//inform the user about success
				final JOptionPane jp = new JOptionPane();
				JOptionPane.showMessageDialog(jp, "Could not save the Ocean" , "Error", JOptionPane.ERROR_MESSAGE);
      
		}
		}

		/**
		 * Things to do when the LoadButton was pressed
		 */
		@SuppressWarnings({ "unchecked", "unused" })
		private void handleLoadButton() {


			// bring up a file chooser
			final JFileChooser chooser = new JFileChooser();
			// set opening path to dir where this app runs
			chooser.setCurrentDirectory(new File("."));
			// get the state of selection
			int selection = chooser.showOpenDialog(gui);
			// if users approves his selection
			if (selection == JFileChooser.APPROVE_OPTION) {
				try {
					// open new FIle stream from the selected file
					FileInputStream is = new FileInputStream(
							chooser.getSelectedFile());
					// create new object stream with this stream
					ObjectInputStream ois = new ObjectInputStream(is);

					// read the new Ocean
					Ocean newOcean = (Ocean) ois.readObject();
				
							// change the current ocean
							controller.setModel(newOcean);

							// pass new ocean to the ocanGraphic
							oceanGraphic.setOcean(controller.getModel());

							// set up the new removeList
							LinkedList<OceanObject> objects = controller.getModel().getOceanObjects();
							removeList.setListData(objects.toArray());
							// set the correct selected object if there is any
							for (int i = 0; i < objects.size();i++) {

								removeList.setSelectedValue(objects.get(i),
										true);
								break;


					}
					// close the stream
					ois.close();

					// Inform user about success
					final JOptionPane optionPane = new JOptionPane();
					JOptionPane.showMessageDialog(optionPane,
							"Succesfully loaded ocean.", "Success",
							JOptionPane.INFORMATION_MESSAGE);
				} catch (Exception ex) {
					// inform user if there was a error reading form the file
					final JOptionPane optionPane = new JOptionPane();
					JOptionPane.showMessageDialog(optionPane,
							"Could not deserialize selection.\n"
									+ "Please choose a correct File!\n",
							"ERROR", JOptionPane.ERROR_MESSAGE);
				}
			}
			

		}

		/**
		 * Things to do when the quit button was pressed
		 */
		private void handleQuitButton() {
			int result = JOptionPane.showConfirmDialog(null,
					"Do you wish to exit the program ?", "Exit Program",
					JOptionPane.YES_NO_OPTION);

			switch (result) {
			case JOptionPane.YES_OPTION:
				System.exit(0);
			}
		}
		/**
         * Things to do when the HelpButton was pressed
         */
        private void handleHelpButton() {
            //Text for the PopUp
            String text = ">>Ocean Life<<\n\n"
            		+ "Welcome to OceanLife by Mirco and Jan!\n\n"
                    + "You can add Objects by pressing the \"Add\" button after\n"
                    + "selecting an object and coordinates.\n"
                    + "You can also edit the coordinates via the sliders.\n\n"
                    + "Beware! If you add a shark he will start eating your fishes.\n\n"
                    + "To select an Object you can click on a name in the list.\n"
                    + "You can remove an selected object by simply\n"
                    + " pressing the \"Remove\" button.\n\n"
                    + "You are also able to clear the whole ocean.";
            //generate Help popup
            final JOptionPane optionPane = new JOptionPane();
            JOptionPane.showMessageDialog(
                            optionPane,
                            text,
                            "Help",
                            JOptionPane.INFORMATION_MESSAGE);
        }

		/**
		 * Things to do when the add button was pressed. Tries to add a new
		 * Object
		 */
		@SuppressWarnings("unchecked")
		private void handleAddButton() {
			OceanObject object = null;
			// get input from gui
			String selection = (String) addBox.getSelectedItem();
			int x = Integer.parseInt(xInput.getText());
			int y = Integer.parseInt(yInput.getText());

			// create object with given position

			if ("Fish".equals(selection)) {

				object = new Fish(x, y, "Fish");

			} else if ("Bubble".equals(selection)) {

				object = new Bubble(x, y, "Bubble");
			} else if ("Stone".equals(selection)) {
				
				object = new Stone(x, controller.getModel().getDepth() - Stone.HEIGHT, "Stone");
			} else if ("Plant".equals(selection)) {

				object = new Plant(x, controller.getModel().getDepth() - Plant.HEIGHT, "Plant");
			} else if ("Shark".equals(selection)) {

				object = new Shark(x, y, "Shark");
			}

			// try to insert created object and update List
			try {
				controller.getModel().addOceanObject(object);
				int i = removeList.getSelectedIndex();
				removeList.setListData(controller.getModel().getOceanObjects().toArray());
				removeList.setSelectedIndex(i);
			} catch (Exception ex) {
				// Generate Error Message when failed
				final JOptionPane optionPane = new JOptionPane();
				JOptionPane.showMessageDialog(optionPane,
						"Couldnt not add Object!", "ERROR",
						JOptionPane.WARNING_MESSAGE);
			}
			
		}

		/**
		 * Things to do when the StepButton was pressed
		 */
		private void handleStepButton() {
			//we have to terminate the Thread when its running to move the Objects step by step
			if(controller.getMoveThread().getState()!=Thread.State.TERMINATED){
                controller.getMoveThread().terminate();
            }
            else{
                controller.getMoveThread().run();                         
            }
		}

		/**
		 * Things to do when removeButton was pressed changes the ocean and the
		 * removeList
		 */
		@SuppressWarnings("unchecked")
		private void handleRemoveButton() {

			// If nothing is selected warn the user and leave this method
			if (-1 == removeList.getSelectedIndex()) {
				final JOptionPane optionPane = new JOptionPane();
				JOptionPane.showMessageDialog(optionPane,
						"Please select an object in order to delete!",
						"Selection error", JOptionPane.WARNING_MESSAGE);
				return;
			}

			// Find the selected object index
			Object selection = (OceanObject) removeList.getSelectedValue();
			LinkedList<OceanObject> objects = controller.getModel().getOceanObjects();
			for (int i = 0; i < objects.size(); i++) {
				if (selection == objects.get(i)) {
					// try to delete it
					try {
						controller.getModel().deleteOceanObject(i);
						int index = removeList.getSelectedIndex();
						removeList.setListData(objects.toArray());
						if (index >= controller.getModel().getOceanObjects().size()) {
							index = controller.getModel().getOceanObjects().size() - 1;
						}
						removeList.setSelectedIndex(index);
					} catch (Exception ex) {
						// Warn user that Object does not exists
						final JOptionPane optionPane = new JOptionPane();
						JOptionPane.showMessageDialog(optionPane,
								"Object not found", "ERROR",
								JOptionPane.WARNING_MESSAGE);
					}
					// if object was found quit the for loop
					break;
				}
			}
			
		}
		
		/**
		 * Things to do when the ClearButoon was pressed
		 */
		private void handleClearButton(){
			//deletes the objects
			controller.getModel().getOceanObjects().clear();
			//creates a new list
			removeList.setListData(new Object[0]);
		}
		
		private void handleDeselectButton(){
			removeList.clearSelection();
			resetBorders();
		}

	}

	/**
     * Nested Listener class for InputFields
     */
    class FocusListenerEx extends FocusAdapter{
        /**
         * Handle inputfields when focus is lost
         * to keep the value in the allowed range
         * a value greater than the ocean width will
         * be reset to the maxOceanwidth
         * a value less than zero will be reset to 0
         * @param e The FocusEvent
         */
        @Override
        public void focusLost(FocusEvent e) {
            Object source = e.getSource();
            if (source == xInput){
                String input = xInput.getText();
                
                try {
                    int i = Integer.parseInt(input);
                    if(i > controller.getModel().getWidth()){
                        xInput.setText(String.valueOf(controller.getModel().getWidth()));
                    }
                    else if(i < 0){
                         xInput.setText("0");
                    }
                } catch (NumberFormatException numberFormatException) {
                    xInput.setText("0");
                }
                xSlider.setValue(Integer.parseInt(xInput.getText()));
            }
            else if(source == yInput){
                String input = yInput.getText();
                try {
                    int i = Integer.parseInt(input);
                    if(i > controller.getModel().getDepth()){
                        yInput.setText(String.valueOf(controller.getModel().getDepth()));
                    }
                    else if(i < 0){
                         xInput.setText("0");
                    }
                } catch (NumberFormatException numberFormatException) {
                    yInput.setText("0");
                }
                ySlider.setValue(Integer.parseInt(yInput.getText()));
            }
        }      
    }
	
	/**
     * Nested Listener class for the Sliders
     */
    class SliderListener implements ChangeListener{
        /**
         * handle a changes selected value
         * to keep the inputFields and the Slider synchronized
         * @param e The ChangeEvent
         */
        @Override
        public void stateChanged(ChangeEvent e) {
            Object source = e.getSource();
            if(source == xSlider){
                xInput.setText(String.valueOf(xSlider.getValue()));
            } 
            else if(source == ySlider){
                yInput.setText(String.valueOf(ySlider.getValue()));
            }
        }
    }
    
	/**
	 * Nested Listener class for input-fields
	 */
	class TextListenerEx implements ActionListener {
		/**
		 * if enter was pressed validate the input-fields and set to allowed
		 * values
		 * 
		 * @param e The ActionEvent
		 */
		@Override
		public void actionPerformed(ActionEvent e) {
			Object source = e.getSource();
			if (source == xInput) {
				String input = xInput.getText();
				try {
					int i = Integer.parseInt(input);
					if (i > controller.getModel().getWidth()) {
						xInput.setText(String.valueOf(controller.getModel().getWidth()));
					} else if (i < 0) {
						xInput.setText("0");
					}
				} catch (NumberFormatException numberFormatException) {
					xInput.setText("0");
				}
				xSlider.setValue(Integer.parseInt(xInput.getText()));
			} else if (source == yInput) {
				String input = yInput.getText();
				try {
					int i = Integer.parseInt(input);
					if (i > controller.getModel().getDepth()) {
						yInput.setText(String.valueOf(controller.getModel().getDepth()));
					} else if (i < 0) {
						yInput.setText("0");
					}
				} catch (NumberFormatException numberFormatException) {
					yInput.setText("0");
				}
				ySlider.setValue(Integer.parseInt(yInput.getText()));
			}
		}
	}
	
	/**
     * Nested Class for the removeList if selection changed
     */
    class ListSelectionListenerEx implements ListSelectionListener{
        /**
         * things to do when selection was changed
         * @param e The ListSelectionEvent
         */
        @Override
        public void valueChanged(ListSelectionEvent e) {
            synchronized(controller.getModel()){
                synchronized(removeButton){
                    //get the selected OceanObject
                   OceanObject object = 
                           (OceanObject)removeList.getSelectedValue();
                   //reset all border flags
                   resetBorders();
                   //find the selected object in the ocean 
                   //and set its border flag
                   LinkedList<OceanObject> objects = controller.getModel().getOceanObjects();
                   for (int i=0;i<objects.size();i++){
                       if(objects.get(i)==object){
                           objects.get(i).setBorder(true);
                           break;
                       }
                   }
                }
            }
        }
    }
    
}
