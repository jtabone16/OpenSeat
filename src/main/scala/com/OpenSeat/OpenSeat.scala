/* OpenSeat.scala
 *
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

  def newField = new TextField{
    text = " "
    columns = 15
  }

  val locationName = newField
  val locationID = newField
  val unitType = newField
  val unitNum = newField

  var avail = new Label()

  new ButtonGroup(mainButtonList: _*)
  new ButtonGroup(locButtonList: _*)

  def top = new MainFrame {
    title = "OpenSeat Admin Panel"
    size = new Dimension(200, 150)
    peer.setLocationRelativeTo(null)

    contents = new BoxPanel(Orientation.Horizontal) {
      contents ++= mainButtonList
      //contents += new FlowPanel(new Label("essss"))
      //background = Color.green
    }


    mainButtonList.foreach(listenTo(_))
    locButtonList.foreach(listenTo(_))
    listenTo(backLoc)
    listenTo(submitNewLoc)

    reactions += {
      case ButtonClicked(button) => {
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