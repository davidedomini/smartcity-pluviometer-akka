package ass03.firestation

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import ass03.Main.computeZone
import ass03.sensors.ZoneLeader
import akka.actor.typed.scaladsl.*

import javax.swing.{JButton, JFrame, JPanel, SwingUtilities}
import java.awt.{BorderLayout, Canvas, Color, Dimension, Graphics}
import ass03.{CityParams, Zone}
import java.awt.event.{ActionEvent, ActionListener}

class Gui(val width: Int, val height: Int, zone: Int, act: ActorRef[FireStation.Command]):
  self =>
  private val frame = JFrame()
  private val canvas = Environment()
  canvas.setSize(width, height)
  frame.setSize(width+200, height+100)
  frame.setTitle("Firestation of zone: " + zone)
  val optPanel = JPanel()
  val manageBtn = JButton("Manage alarm")
  manageBtn.addActionListener( new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit =
      act ! FireStation.ManageAlarm
  })
  val resolveBtn = JButton("Resolve alarm")
  resolveBtn.addActionListener( new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit =
      act ! FireStation.ResolveAlarm
  })
  optPanel.add(manageBtn)
  optPanel.add(resolveBtn)
  val layout = BorderLayout()
  frame.setLayout(layout)
  frame.setLocationRelativeTo(null)
  frame.add(canvas, BorderLayout.NORTH)
  frame.add(optPanel, BorderLayout.SOUTH)
  frame.setVisible(true)
  canvas.setVisible(true)

  def render(elements: List[(Zone, String)]): Unit = SwingUtilities.invokeLater { () =>
    canvas.elements = elements
    canvas.invalidate()
    canvas.repaint()
  }

  private class Environment extends JPanel:
    var elements: List[(Zone, String)] = List.empty
    override def getPreferredSize = new Dimension(self.width, self.height)

    override def paintComponent(g: Graphics): Unit =
      g.clearRect(0, 0, self.width, self.height)
      for
        e <- elements
        z = e._1
        alarm = e._2
        xs = z.x + (z.offsetX/3).toInt
        ys = z.y + (z.offsetY/2).toInt
      do
        alarm match{
          case "NoAlarm" => g.setColor(Color.GREEN)
          case "UnderManagement" => g.setColor(Color.YELLOW)
          case "Alarm" => g.setColor(Color.RED)
        }
        g.fillRect(z.x, z.y, z.offsetX, z.offsetY)
        g.setColor(Color.BLACK)
        g.drawString("Zone: " + z.index, xs, ys)


object TryGui extends App:
  val g = Gui(600, 300, 1, null)
  val world = CityParams(600, 200, 2, 3, 20, 31)
  val zones = computeZone(world)
  g.render(zones.map(z => if z.index % 2 == 0 then (z, "NoAlarm") else (z, "Alarm")).toList)