package barneshut

import org.scalatest.FunSuite

class SimulatorSuite extends FunSuite {

  test("testUpdateBoundaries") {
    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 10
    boundaries.maxY = 17
    simulator.updateBoundaries(boundaries, body)
    assert(boundaries.minX === 1 && boundaries.minY === 1 && boundaries.maxX === 25 && boundaries.maxY === 47)
  }

  test("testMergeBoundaries") {
    val model = new SimulationModel
    val simulator = new Simulator(model.taskSupport, model.timeStats)
    val a = new Boundaries()
    val b = new Boundaries()
    a.minX = 1
    a.minY = 1
    a.maxX = 30
    a.maxY = 27
    b.minX = -1
    b.minY = -1
    b.maxX = 10
    b.maxY = 17
    val r = simulator.mergeBoundaries(a, b)
    assert(a.minX === 1 && a.minY === 1 && a.maxX === 30 && a.maxY === 27)
    assert(b.minX === -1 && b.minY === -1 && b.maxX === 10 && b.maxY === 17)
    assert(r.minX === -1 && r.minY === -1 && r.maxX === 30 && r.maxY === 27)


  }

}
