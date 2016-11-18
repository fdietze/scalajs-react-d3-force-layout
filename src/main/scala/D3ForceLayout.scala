package fdietze.scalajs.react.components

import collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js.Dynamic.global
import scala.annotation.meta.field
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import org.singlespaced.d3js
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.{Link, ZoomEvent}
import org.singlespaced.d3js.forceModule.Force
import org.singlespaced.d3js.behavior.Zoom

import js.JSConverters._
import pharg._
import vectory._

trait D3ForceLayout[V, P] {

  class D3Vertex(
    val v: V
  ) extends d3js.forceModule.Node

  class D3Edge(
    val e: Edge[V],
    @(JSExport @field) var source: D3Vertex,
    @(JSExport @field) var target: D3Vertex
  ) extends d3js.Link[D3Vertex]

  type VertexSelection = d3js.selection.Update[D3Vertex]
  type EdgeSelection = d3js.selection.Update[D3Edge]

  case class Props(
    graph: DirectedGraph[V],
    dimensions: Vec2,
    private val _props: Option[P] = None
  ) {
    lazy val props = _props.get
  }

  def shouldUpdateForce(current:Props, next:Props) = true
  def shouldUpdateGraph(current:Props, next:Props) = current.graph != next.graph
  def shouldUpdateVisualization(current:Props, next:Props) = true

  case class State(
    force: Force[D3Vertex, D3Edge],
    var zoom: Zoom[Unit],
    var vertexData: js.Array[D3Vertex],
    var edgeData: js.Array[D3Edge],
    var vertexSel: js.UndefOr[VertexSelection] = js.undefined,
    var edgeSel: js.UndefOr[EdgeSelection] = js.undefined
  )

  def chargeDistance(p: Props): Double = Double.PositiveInfinity
  def theta: Double = 0.8
  def gravity(p: Props): Double = 0.1
  def friction: Double = 0.9
  def initialSize: (Double,Double) = (100.0, 100.0)

  //TODO: provide a way to set a constant instead of a function
  def charge(p: Props, v: V): Double = -30
  def linkDistance(p: Props, e: Edge[V]): Double = 20
  def linkStrength(p: Props, e: Edge[V]): Double = 3

  def updateForce(p: Props, force:Force[D3Vertex, D3Edge]):Force[D3Vertex, D3Edge] = {
    import p._
    force
        .charge((d: D3Vertex, _: Double) => charge(p, d.v))
        .chargeDistance(chargeDistance(p))
        .theta(theta)
        .gravity(gravity(p))
        .friction(friction)
        .linkDistance((d: D3Edge, _: Double) => linkDistance(p, d.e))
        .linkStrength((d: D3Edge, _: Double) => linkStrength(p, d.e))
        .size(initialSize)

    force
  }

  private def initialState(p: Props): State = {
    import p._
    State(
      force = updateForce(p, d3.layout.force[D3Vertex, D3Edge]()),
      zoom = d3.behavior.zoom(),
      vertexData = js.Array(),
      edgeData = js.Array()
    )
  }

  val containerRef = Ref[raw.HTMLElement]("container")
  val vertexGroupRef = Ref[raw.SVGElement]("vertices")
  val edgeGroupRef = Ref[raw.SVGElement]("edges")

  def renderVertexArea(p: Props, s: State) = {
    import p._
  }

  def renderEdgeArea(p: Props, s: State) = {
    import p._
  }


  def render(p: Props, s: State) = {
    import p._
    <.div(
      ^.ref := "container",
      ^.width := "100%",
      ^.height := "100%",
      ^.position := "relative",
      <.svg.svg(
        ^.width := "100%",
        ^.height := "100%",
        ^.position := "absolute",
        <.svg.g(
          ^.ref := "edges"
        ),
        <.svg.g(
          ^.ref := "vertices"
        )
      )
    )
  }

  def vertexElement = "circle"
  def edgeElement = "line"

  def positionVertices(sel: VertexSelection): VertexSelection = {
    sel
      .attr("cx", (d: D3Vertex) => d.x)
      .attr("cy", (d: D3Vertex) => d.y)
  }

  def positionEdges(sel: EdgeSelection): EdgeSelection = {
    sel
      .attr("x1", (d: D3Edge) => d.source.x)
      .attr("y1", (d: D3Edge) => d.source.y)
      .attr("x2", (d: D3Edge) => d.target.x)
      .attr("y2", (d: D3Edge) => d.target.y)
  }

  def styleVertices(p: Props, sel: VertexSelection): VertexSelection = {
    sel
      .attr("r", (d: D3Vertex) => 5)
      .style("fill", "steelblue")
  }

  def styleEdges(p: Props, sel: EdgeSelection): EdgeSelection = {
    sel
      .style("stroke", "#666")
      .style("stroke-width", 2)
  }


  val reuseVertexCoordinatesOnUpdate = false
  val panAndZoom = true
  val minZoom: Double = 0.1
  val maxZoom: Double = 10

  class Backend($: BackendScope[Props, State]) {

    def updateData(p: Props, s: State) = Callback {
      import p._
      import s._

      val newVertices = p.graph.vertices
      val oldVertices = vertexData.map(d => d.v -> d).toMap

      val newD3Vertices = if (reuseVertexCoordinatesOnUpdate) {
        var removedD3Vertices: List[D3Vertex] = vertexData.filterNot(d => newVertices contains d.v).toList
        newVertices.map { v =>
          oldVertices.getOrElse(v, {
            val newD3V = new D3Vertex(v)
            removedD3Vertices.headOption.foreach { removed =>
              newD3V.x = removed.x
              newD3V.y = removed.y
              newD3V.px = removed.px
              newD3V.py = removed.py
              removedD3Vertices = removedD3Vertices.tail
            }
            newD3V
          })
        }.toJSArray
      }
      else
        newVertices.map { v => oldVertices.getOrElse(v,  new D3Vertex(v) ) }.toJSArray


      val vertexMap = newD3Vertices.map(d => d.v -> d).toMap
      val oldEdges = edgeData.map(d => (d.e -> d)).toMap
      val newEdges = p.graph.edges.map { e =>
        oldEdges.getOrElse(e, new D3Edge(e, vertexMap(e.in), vertexMap(e.out)))
      }.toJSArray

      vertexData = newD3Vertices
      edgeData = newEdges
    }

    def updateVisualization(p: Props, s: State) = Callback {
      import p._
      import s._

      lazy val domVerticesSel = d3.select(vertexGroupRef($).get).selectAll(vertexElement)
      lazy val domEdgesSel = d3.select(edgeGroupRef($).get).selectAll(edgeElement)
      if (panAndZoom) {
        zoom = zoom.scaleExtent((minZoom, maxZoom)).on("zoom", zoomed _)
        d3.select(containerRef($).get).call(zoom)
      }

      vertexSel = vertexSel.getOrElse(domVerticesSel).data(vertexData)
      for (v <- vertexSel) {
        v.exit().remove()
        v.enter().append(vertexElement)
        styleVertices(p, v)
        positionVertices(v)
      }

      edgeSel = edgeSel.getOrElse(domEdgesSel).data(edgeData)
      for( e <- edgeSel) {
        e.exit().remove()
        e.enter().append(edgeElement)
        styleEdges(p, e)
        positionEdges(e)
      }

    }

    def initForce(p:Props, s:State) = Callback {
      import p._
      import s._

      updateForce(p, force)

      force.nodes(vertexData).links(edgeData)
      force.start()
    }


    def zoomed(y: Unit, x: Double) {
      val vertexContainer = d3.select(vertexGroupRef($).get)
      val edgeContainer = d3.select(edgeGroupRef($).get)

      val e = d3.event.asInstanceOf[ZoomEvent]
      vertexContainer.attr("transform", "translate(" + e.translate + ")scale(" + e.scale + ")")
      edgeContainer.attr("transform", "translate(" + e.translate + ")scale(" + e.scale + ")")
    }

    def update(p: Props, s: State): Callback = updateData(p, s) >> updateVisualization(p, s)

    def registerTick(s: State) = Callback {
      import s._

      force.on("tick", (e: Event) => {
        positionVertices(vertexSel.get)
        positionEdges(edgeSel.get)
        ()
      })
    }

    def stopForce(s: State) = Callback {
      s.force.stop()
    }

    def moveOldCenterToNewCenter(currentDim:Vec2, nextDim:Vec2, s: State) {
      import s._

      val centerDiff = (nextDim - currentDim) / 2
      val newTranslate = Vec2(zoom.translate()) + centerDiff
      zoom.translate(newTranslate.toTuple)

      val vertexContainer = d3.select(vertexGroupRef($).get)
      val edgeContainer = d3.select(edgeGroupRef($).get)
      vertexContainer.attr("transform", "translate(" + zoom.translate() + ")scale(" + zoom.scale() + ")")
      edgeContainer.attr("transform", "translate(" + zoom.translate() + ")scale(" + zoom.scale() + ")")
    }
  }

  protected val component = ReactComponentB[Props]("SmartComponent")
    .initialState_P(initialState)
    .backend(new Backend(_))
    .renderPS((_, p, s) => render(p, s))
    .componentDidMount(c => c.backend.update(c.props, c.state) >> c.backend.initForce(c.props, c.state) >> c.backend.registerTick(c.state))
    .shouldComponentUpdate(c => {
      if( c.nextProps != c.currentProps ) {
        if(shouldUpdateGraph(c.currentProps, c.nextProps)) {
          c.$.backend.updateData(c.nextProps, c.nextState).runNow()
          c.$.backend.updateVisualization(c.nextProps, c.nextState).runNow()
          c.$.backend.initForce(c.nextProps, c.nextState).runNow()
          c.$.backend.registerTick(c.currentState).runNow()
        } else {

          if(shouldUpdateVisualization(c.currentProps, c.nextProps) ) {
            c.$.backend.updateVisualization(c.nextProps, c.nextState).runNow()
          }

          if( shouldUpdateForce(c.currentProps, c.nextProps))
            c.$.backend.initForce(c.nextProps, c.nextState).runNow()
        }

        if( c.nextProps.dimensions != c.currentProps.dimensions )
          c.$.backend.moveOldCenterToNewCenter(c.currentProps.dimensions, c.nextProps.dimensions, c.nextState)
      }
      false // let d3 handle the update, instead of react
    })
    .componentWillUnmount(c => c.backend.stopForce(c.state))
    .build

  def apply(graph: DirectedGraph[V], dimensions: Vec2, props:Option[P]) = component(Props(graph, dimensions, props))
}
