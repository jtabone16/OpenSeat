/* OpenSeatClasses.scala
 * 
 * 'Location' class declaration with 'Unit' class defined as
 *  an inner class.
 *
 * 'User' class declaration.
 *
 *  Created by John Tabone on 4/8/14
 */

 // Note: Think about using 'companion object' to assign random ints for IDs

package OpenSeatClasses

import scala.collection
import scala.io.Source._

//Class used for locations
class Location { loc =>

  //Location name and ID
  var name:String = null
  var ID:Int = 0

  //Auxiliary constructor for Location class
  def this(name:String, ID:Int) {
    this()
    this.name = name
    this.ID = ID
  }

  //Declare inner class called 'Unit' that states the type
  //of unit, number of units, vacant units, and occupied
  //units. By declaring an inner or nested class, each declaration
  //of a unit is specific to each location.
  class Unit{
    var UnitType:String = null
    var NumUnits, VacantUnits, OccupiedUnits:Int = 0
    var NumString, VacantString:String = null //

    //Auxiliary constructor for 'Unit' class
    def this(UnitType:String, NumUnits:Int){
      this()
      this.UnitType = UnitType
      this.NumUnits = NumUnits
      this.VacantUnits = NumUnits
      this.NumString = "There are " + NumUnits + " total " + UnitType + "(s) in" + loc.name
    }

    //Update number of available units when a unit is occupied
    def occupyUnit: String = {
      if (OccupiedUnits < NumUnits){
        OccupiedUnits+=1
        VacantUnits-=1
        VacantString = null //empty the string
        VacantString = "There are " + VacantUnits + " (of " + NumUnits + ") available " + UnitType + "(s) in" + loc.name
      }
      else{
        VacantString = null //empty the string
        VacantString = "All " + UnitType + "(s) in " + loc.name + " are occupied!"
      }
    }//occupyUnit

    //Update number of available units when a unit is vacated
    def vacateUnit: String = {
      if (VacantUnits < NumUnits){
        VacantUnits+=1
        OccupiedUnits-=1
        VacantString = null //empty the string
        VacantString = "There are " + VacantUnits + " (of " + NumUnits + ") available " + UnitType + "(s) in" + loc.name  
      }
      else{
        VacantString = null //empty the string
        VacantString = "All " + UnitType + "(s) in " + loc.name + " are available!"
      }
    }//vacateUnit

  }//Unit

  //List of different units in each location
  var units = scala.collection.mutable.ArrayBuffer[Unit]()

  //Define a new type of unit (e.g. computer, seat, treadmill, etc.)
  //and add to list of units in each location
  def newUnit(UnitType:String, NumUnits:Int): Unit = {
    val TempUnit = new Unit(UnitType, NumUnits)
    units+=TempUnit
    TempUnit
  }

}//Location


//Class used for users
class User{
  var ID, NumCheckIns:Int = 0
  var name, FavLocation, FavUnit:String = null
  var LocationsVisited = scala.collection.mutable.ArrayBuffer[String]()

}//User

