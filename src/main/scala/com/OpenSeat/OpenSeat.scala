/* OpenSeat.scala
 * 
 * A backend GUI for an admin user of the OpenSeat service. OpenSeat uses crowdsourcing
 * to track vacancies of 'units' in various public locations such as labs, libraries, gyms,
 * coffee shops, etc. We used the Scala flavor of the Swing library to create a simple GUI.
 *
 * With this GUI, one can add, edit, and view locations. Currently, we make sure location names
 * are unique (we don't do so for location ID). When editing a location, you're not actually editing
 * a location name or ID. But you can add a unit for each location. You can also occupy or vacate seats
 * in 'Edit Location'. When doing so, a string with information about the available units in that specific
 * location is outputted to Terminal. 
 * 
 * We had some issues with 'Edit Location', mainly with the window updating after making an edit. Our brute force 
 * remedy involves using the back button and going back into the 'Edit Location' window. 
 * 
 * Also, we had issues saving our location and unit info after closing the program. There were certain 
 * libraries that saved data structures, but the learning curve was quite steep for what we thought was a trivial
 * matter in this project (considering that in production all of this would be handled with a database e.g. SQL, Redis, etc.)
 *
 * The creating and editing of locations and units is handled by OpenSeatClasses.scala which includes a Location class
 * with a nested Unit class and a User class which will be implemented in a future version of the program. Since the use case
 * of this GUI is for admins of the service, we focused more on manipulating locations and units rather than users.
 *
 * Created by John Tabone on 4/8/14
 */

import scala.collection
import scala.io.Source._
import OpenSeatClasses._
import scala.swing._
import swing.event._
import java.awt.{ Color, Graphics2D }
import util.control.Breaks._


 object OpenSeat extends SimpleSwingApplication{

  //list of locations
  var locations = scala.collection.mutable.Map[String, Location]()

  //Main screen button group
  val mainButtonList = List(
    new Button(){ 
      name = "loc"
      text = "Locations"
    },
    new Button(){ 
      name = "user"
      text = "Users"
    }
  )

  //Location screen button group
  val locButtonList = List(
    new Button(){ 
      name = "viewloc"
      text = "View Locations"
    },
    new Button(){ 
      name = "newloc"
      text = "New Location"
    },
    new Button(){
      name = "editloc"
      text = "Edit Location"
    },
    new Button(){
      name = "back"
      text = "Back"
    }
  )

  //Button for submitting a new location
  val submitNewLoc = new Button(){ 
    name = "submitnewloc"
    text = "Submit"
  }

  //Button to back up to 'Location' page
  val backLoc = new Button(){ 
    name = "backloc"
    text = "Back"
  }

  //Default text field
  def newField = new TextField{
    text = " "
    columns = 15
  }

  //Declarations of various text fields
  val locationName = newField
  val locationID = newField
  val unitType = newField
  val unitNum = newField

  //Label for available seats
  var avail = new Label()

  //Button groups to be used in the GUI
  new ButtonGroup(mainButtonList: _*)
  new ButtonGroup(locButtonList: _*)

  //The top function is where all the magic happens when using Swing. Think of it 
  //as a main() function
  def top = new MainFrame {
    title = "OpenSeat Admin Panel"  //set title of the window
    size = new Dimension(200, 150) //set dimensions of the window
    peer.setLocationRelativeTo(null) //center the window 

    //Add our main button list/group to our main window
    contents = new BoxPanel(Orientation.Horizontal) {
      contents ++= mainButtonList
      //contents += new FlowPanel(new Label("essss"))
      //background = Color.green
    }

    //Add event listeners for each button group and button
    mainButtonList.foreach(listenTo(_))
    locButtonList.foreach(listenTo(_))
    listenTo(backLoc)
    listenTo(submitNewLoc)

    //This is where we handle events
    reactions += {
      //When button is clicked
      case ButtonClicked(button) => {
        //case is the 'name' attribute of a button
        button.name match {
          
          case "loc" => {
            preferredSize = new Dimension(500, 200)
            contents = new BoxPanel(Orientation.Horizontal) { contents ++= locButtonList }
          }
          
          case "user" => println("Feature will be added in next version!")
          
          case "back" => {
            preferredSize = new Dimension(200, 150)
            contents = new BoxPanel(Orientation.Horizontal) { contents ++= mainButtonList }
          }
          
          case "backloc" => {
            preferredSize = new Dimension(500, 200)
            contents = new BoxPanel(Orientation.Horizontal) { contents ++= locButtonList; title = "OpenSeat Admin Panel"}
          }
          
          case "newloc" => {
            title = "Add new location"
            contents = new FlowPanel(new Label("Location Name: "), locationName, new Label("Location ID: "), locationID, submitNewLoc, backLoc)
          }//newloc

          case "submitnewloc" =>{
            if (locationName.text == " ") {
              locationName.text = "Please enter a location name"
              break
            }
            else if (locationID.text == " ") {
              locationID.text = "Please enter a location ID"
              break
            }
            else if (!locationID.text.forall(_.isDigit)) {
              locationID.text = "Please enter an integer"
              break
            }

            val location = new Location(locationName.text, Integer.parseInt(locationID.text))
            if (!locations.contains(locationName.text)) {
              locations(locationName.text) = location

            }
            else{
              locationName.text = "Please enter a NEW location name"
              break
            }
            locationName.text = " "
            locationID.text = " "
          }//submitnewloc
          
          case "viewloc" => {
            title = "View locations"
            contents = new BoxPanel(Orientation.Vertical){
              contents+= new FlowPanel(
                new Label{text = "Location Name"; font = new Font("Ariel", java.awt.Font.BOLD, 14)}, 
                new Label{text = "Location ID"; font = new Font("Ariel", java.awt.Font.ITALIC, 14)})
              locations.foreach{
                loc=>
                  contents += new FlowPanel(
                    new Label{text = loc._2.name; font = new Font("Ariel", java.awt.Font.BOLD, 14)}, 
                    new Label{text = loc._2.ID.toString; font = new Font("Ariel", java.awt.Font.ITALIC, 14)}
                    )
              }
              contents += new FlowPanel(backLoc)
            }  
          }//viewloc

          case "editloc" => {
            title = "Edit locations"
            preferredSize = new Dimension(650, 150)
            contents = new BoxPanel(Orientation.Vertical){
              locations.foreach{
                loc=>
                  contents += new FlowPanel(new Label(loc._2.name), new Label(loc._2.ID.toString), new Button()
                  {
                    name = loc._2.name
                    action = Action("Add unit to" + loc._2.name){
                        contents += new FlowPanel(new Label("Unit Type: "), unitType, new Label(" Number of units: "), unitNum, new Button() {
                        action = Action("Submit"){                        
                          if (unitType.text == " ") {
                            unitType.text = "Please enter a unit type"
                            break
                          }
                          else if(unitNum.text == " "){
                            unitNum.text = "Please enter the number of units"
                            break
                          }
                          else if(!unitNum.text.forall(_.isDigit)){
                            unitNum.text = "Please enter an integer"
                            break
                          }

                          if (!loc._2.units.contains(unitType.text)) {
                            loc._2.newUnit(unitType.text, Integer.parseInt(unitNum.text))
                          }
                          else{
                            unitType.text = "Please enter a NEW unit type"
                            break
                          }

                          unitType.text = " "
                          unitNum.text = " "

                        }//submit action for new unit type for a specific location
                        })
                    }
                  }) //Add an action that adds a new unit to this location
                  if (!loc._2.units.isEmpty){
                    loc._2.units.foreach{
                      unit=>
                        avail = new Label(unit._2.VacantUnits.toString)

                        contents += new FlowPanel(
                          new Label{text = "Unit:"; font = new Font("Ariel", java.awt.Font.BOLD, 12)},
                          new Label(unit._2.UnitType), 
                          new Label{text = " Number of units:"; font = new Font("Ariel", java.awt.Font.BOLD, 12)}, 
                          new Label(unit._2.NumUnits.toString),
                          new Label{text = " Available units:"; font = new Font("Ariel", java.awt.Font.BOLD, 12)}, 
                          avail
                        )//unit info

                        contents+= new FlowPanel(
                          new Button(){
                            action = Action("Occupy unit"){
                              unit._2.occupyUnit                              
                              println(unit._2.VacantString)
                            }
                          },
                          new Button(){
                            action = Action("Vacate unit"){                            
                              unit._2.vacateUnit                              
                              println(unit._2.VacantString)
                            }
                          }
                        )//occupy and vacate buttons

                    }//foreach unit in a location
                  }//if a location has more than 0 units
              }//foreach location
              contents += new FlowPanel(backLoc)
            }
          }//editloc
        }//button.name match
      }//case ButtonClicked
    }//reactions
  }//top

 }//OpenSeat