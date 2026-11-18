package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

/**
 * Pruebas de contrato de la versión secuencial: una por función, con los
 * valores de los dos ejemplos del enunciado. No se modifican.
 *
 * La rúbrica pide además cinco casos propios por función, distintos de estos y
 * de los ejemplos del enunciado. Van al final de este archivo o en un archivo
 * nuevo de esta misma carpeta; las dos formas valen.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudTest extends AnyFunSuite {

  val eps = 1e-9

  // Ejemplo 1 del enunciado, con las palabras ya normalizadas para que estas
  // pruebas no dependan unas de otras.
  val d0: Documento = ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa"))
  val d1: Documento = ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra"))
  val d2: Documento = ("d2", Vector("la", "lluvia", "moja", "el", "parque"))
  val c1: Corpus = Vector(d0, d1, d2)

  // Ejemplo 2 del enunciado.
  val e0: Documento = ("e0", Vector("la", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e1: Documento = ("e1", Vector("toda", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e2: Documento = ("e2", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "tareas", "independientes"))
  val e3: Documento = ("e3", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "hilos", "independientes"))
  val c2: Corpus = Vector(e0, e1, e2, e3)

  test("normalizar: baja a minúsculas y parte en palabras") {
    assert(normalizar("El Gato Duerme") == Vector("el", "gato", "duerme"))
  }

  test("normalizar: puntuación y mayúsculas") {
    assert(Similitud.normalizar("El gato, duerme!") == Vector("el", "gato", "duerme"))
  }
  test("normalizar: espacios múltiples") {
    assert(Similitud.normalizar("a   b") == Vector("a", "b"))
  }
  test("normalizar: texto vacío") {
    assert(Similitud.normalizar("") == Vector())
  }
  test("normalizar: solo signos") {
    assert(Similitud.normalizar("...,;!") == Vector())
  }
  test("normalizar: dígitos") {
    assert(Similitud.normalizar("Punto 3: ok") == Vector("punto", "3", "ok"))
  }

  test("fragmentos: son las ventanas consecutivas, en orden") {
    val ts = Vector("a", "b", "c", "d")
    assert(fragmentos(ts, 2) ==
      Vector(Vector("a", "b"), Vector("b", "c"), Vector("c", "d")))
  }

  test("fragmentos: ejemplo del enunciado") {
    val ts = Vector("el", "gato", "duerme", "en", "el", "sofa")
    assert(Similitud.fragmentos(ts, 3).length == 4)
    assert(Similitud.fragmentos(ts, 3).head == Vector("el", "gato", "duerme"))
  }
  test("fragmentos: menos de k palabras") {
    assert(Similitud.fragmentos(Vector("a", "b"), 3) == Vector())
  }
  test("fragmentos: exactamente k palabras") {
    assert(Similitud.fragmentos(Vector("a", "b", "c"), 3) == Vector(Vector("a", "b", "c")))
  }
  test("fragmentos: k = 1") {
    assert(Similitud.fragmentos(Vector("a", "b"), 1) == Vector(Vector("a"), Vector("b")))
  }
  test("fragmentos: vector vacío") {
    assert(Similitud.fragmentos(Vector(), 2) == Vector())
  }

  test("huella: el ejemplo 1 tiene 4, 4 y 3 fragmentos") {
    assert(huella(d0, 3).size == 4)
    assert(huella(d1, 3).size == 4)
    assert(huella(d2, 3).size == 3)
  }

  test("huellas: hay una huella por documento") {
    assert(huellas(c1, 3).length == 3)
  }

  test("jaccard: ejemplo 2, las dos parejas dan 0.75 y 0.5") {
    assert(math.abs(jaccard(huella(e0, 3), huella(e1, 3)) - 0.75) < eps)
    assert(math.abs(jaccard(huella(e2, 3), huella(e3, 3)) - 0.5) < eps)
  }

  test("parejas: hay n (n - 1) / 2") {
    assert(parejas(5).length == 10)
  }

  test("similitudes: el ejemplo 1 da 1/3, 0 y 0") {
    val hs = huellas(c1, 3)
    val ms = similitudes(hs, parejas(3)).map(_._3)
    assert(math.abs(ms(0) - 2.0 / 6.0) < eps && ms(1) == 0.0 && ms(2) == 0.0)
  }

  // --- Pruebas propias para 2.8: similitudes ---
  test("similitudes propio 1: documentos con huellas idénticas dan similitud 1.0") {
    val hA: Huella = Set(Vector("a", "b"), Vector("b", "c"))
    val hs: Vector[Huella] = Vector(hA, hA)
    val ms = similitudes(hs, Vector((0, 1)))
    assert(ms.length == 1 && ms.head._1 == 0 && ms.head._2 == 1 && math.abs(ms.head._3 - 1.0) < eps)
  }

  test("similitudes propio 2: documentos totalmente disjuntos dan similitud 0.0") {
    val hA: Huella = Set(Vector("a", "b"))
    val hB: Huella = Set(Vector("x", "y"))
    val hs: Vector[Huella] = Vector(hA, hB)
    val ms = similitudes(hs, Vector((0, 1)))
    assert(ms == Vector((0, 1, 0.0)))
  }

  test("similitudes propio 3: solapamiento parcial calcula el Jaccard exacto") {
    val hA: Huella = Set(Vector("a"), Vector("b"), Vector("c"))
    val hB: Huella = Set(Vector("b"), Vector("c"), Vector("d"))
    val hs: Vector[Huella] = Vector(hA, hB)
    val ms = similitudes(hs, Vector((0, 1)))
    assert(ms.length == 1 && ms.head._1 == 0 && ms.head._2 == 1 && math.abs(ms.head._3 - 0.5) < eps)
  }

  test("similitudes propio 4: preserva el orden exacto de las parejas consultadas") {
    val h: Huella = Set(Vector("a"))
    val hs: Vector[Huella] = Vector(h, h, h)
    val ps = Vector((1, 2), (0, 2), (0, 1))
    val ms = similitudes(hs, ps)
    assert(ms.map(m => (m._1, m._2)) == ps)
  }

  test("similitudes propio 5: vector de parejas vacío devuelve vector vacío") {
    val hs: Vector[Huella] = Vector(Set(Vector("a")), Set(Vector("b")))
    assert(similitudes(hs, Vector.empty[Pareja]).isEmpty)
  }

  test("indiceInvertido: el fragmento inicial de d0 está en d0 y en d1") {
    val idx = indiceInvertido(huellas(c1, 3))
    assert(idx(Vector("el", "gato", "duerme")) == Set(0, 1))
  }

  test("parejasCandidatas: en el ejemplo 2 quedan dos de seis") {
    val idx = indiceInvertido(huellas(c2, 3))
    assert(parejasCandidatas(idx) == Vector((0, 1), (2, 3)))
  }

  test("ranking: ordena de mayor a menor y trunca a p") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(ms, 0.0, 10).map(_._3) == Vector(0.9, 0.4, 0.1))
    assert(ranking(ms, 0.0, 2).length == 2)
  }

  // --- Pruebas propias para 2.11: ranking ---
  test("ranking propio 1: filtra estrictamente por umbral descartando menores") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.8), (0, 2, 0.45), (1, 2, 0.6), (1, 3, 0.2))
    assert(ranking(ms, 0.5, 10) == Vector((0, 1, 0.8), (1, 2, 0.6)))
  }

  test("ranking propio 2: desempata por posición lexicográfica (i, j) ante similitudes idénticas") {
    val ms: Vector[Medicion] = Vector((1, 2, 0.7), (0, 3, 0.7), (0, 1, 0.7), (0, 2, 0.7))
    assert(ranking(ms, 0.5, 10) == Vector((0, 1, 0.7), (0, 2, 0.7), (0, 3, 0.7), (1, 2, 0.7)))
  }

  test("ranking propio 3: trunca exactamente a p cuando hay más candidatos que el tope") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.9), (0, 2, 0.85), (0, 3, 0.8), (1, 2, 0.75), (1, 3, 0.7))
    assert(ranking(ms, 0.7, 3) == Vector((0, 1, 0.9), (0, 2, 0.85), (0, 3, 0.8)))
  }

  test("ranking propio 4: devuelve todos los que superan el umbral cuando p es mayor a los disponibles") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.95), (2, 3, 0.8))
    assert(ranking(ms, 0.5, 100) == Vector((0, 1, 0.95), (2, 3, 0.8)))
  }

  test("ranking propio 5: maneja vector vacío o cuando ningún par supera el umbral") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.1), (0, 2, 0.2))
    assert(ranking(ms, 0.5, 5).isEmpty)
    assert(ranking(Vector.empty[Medicion], 0.0, 5).isEmpty)
  }

  test("detectar: el ejemplo 2 reporta (0, 1) con 0.75 y (2, 3) con 0.5") {
    val r = detectarDirecto(c2, 3, 0.3, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(r(0)._3 - 0.75) < eps && math.abs(r(1)._3 - 0.5) < eps)
  }

  test("detectar: las dos soluciones coinciden en los ejemplos") {
    assert(detectarDirecto(c1, 3, 0.2, 5) == detectarConIndice(c1, 3, 0.2, 5))
    assert(detectarDirecto(c2, 3, 0.3, 5) == detectarConIndice(c2, 3, 0.3, 5))
  }

  // --- Pruebas propias para 2.12: detectarDirecto y detectarConIndice ---
  val docA: Documento = ("docA", Vector("programacion", "funcional", "en", "scala", "es", "declarativa"))
  val docB: Documento = ("docB", Vector("programacion", "funcional", "en", "scala", "es", "expresiva"))
  val docC: Documento = ("docC", Vector("redes", "neuronales", "profundas", "en", "python"))
  val corpusTest: Corpus = Vector(docA, docB, docC)

  test("detectarDirecto propio 1: detecta alta similitud entre documentos con prefijo común") {
    val res = detectarDirecto(corpusTest, 3, 0.5, 5)
    assert(res.length == 1)
    assert(res.head._1 == 0 && res.head._2 == 1)
  }

  test("detectarDirecto propio 2: umbral alto descarta parejas con similitud moderada") {
    val res = detectarDirecto(corpusTest, 3, 0.9, 5)
    assert(res.isEmpty)
  }

  test("detectarDirecto propio 3: respeta el tope p de parejas más similares") {
    val dX: Documento = ("dX", Vector("mismo", "texto", "repetido", "siempre"))
    val cIdenticos: Corpus = Vector(dX, dX, dX, dX)
    val res = detectarDirecto(cIdenticos, 2, 0.5, 2)
    assert(res.length == 2)
    assert(math.abs(res.head._3 - 1.0) < eps)
  }

  test("detectarDirecto propio 4: corpus sin fragmentos comunes retorna vector vacío") {
    val cDisjunto: Corpus = Vector(
      ("d1", Vector("perro", "corre")),
      ("d2", Vector("pez", "nada")),
      ("d3", Vector("ave", "vuela"))
    )
    val res = detectarDirecto(cDisjunto, 2, 0.1, 5)
    assert(res.isEmpty)
  }

  test("detectarDirecto propio 5: desempata parejas con igual similitud por índices (i, j)") {
    val dP: Documento = ("dP", Vector("a", "b", "c"))
    val cP: Corpus = Vector(dP, dP, dP)
    val res = detectarDirecto(cP, 2, 0.0, 3)
    assert(res.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2), (1, 2)))
  }

  test("detectarConIndice propio 1: produce exactamente el mismo ranking que detectarDirecto") {
    assert(detectarDirecto(corpusTest, 3, 0.3, 5) == detectarConIndice(corpusTest, 3, 0.3, 5))
  }

  test("detectarConIndice propio 2: corpus disjunto no genera candidatos y retorna vacío") {
    val cDisjunto: Corpus = Vector(
      ("d1", Vector("alfa", "beta")),
      ("d2", Vector("gamma", "delta")),
      ("d3", Vector("epsilon", "zeta"))
    )
    assert(detectarConIndice(cDisjunto, 2, 0.1, 5).isEmpty)
  }

  test("detectarConIndice propio 3: umbral 1.0 solo detecta copias idénticas") {
    val dX: Documento = ("dX", Vector("palabra", "uno", "dos", "tres"))
    val cMixto: Corpus = Vector(docA, docB, dX, dX)
    val res = detectarConIndice(cMixto, 2, 1.0, 5)
    assert(res.length == 1)
    assert(res.head._1 == 2 && res.head._2 == 3 && math.abs(res.head._3 - 1.0) < eps)
  }

  test("detectarConIndice propio 4: respeta truncamiento a p elementos") {
    val dP: Documento = ("dP", Vector("comun", "uno", "dos", "tres"))
    val cCuatro: Corpus = Vector(dP, dP, dP, dP)
    val res = detectarConIndice(cCuatro, 2, 0.5, 1)
    assert(res.length == 1)
  }

  test("detectarConIndice propio 5: coincide con directo usando corpusConCopias") {
    val cCopias = corpusConCopias(4, 30, 8, 2, 0.7)
    assert(detectarConIndice(cCopias, 3, 0.2, 5) == detectarDirecto(cCopias, 3, 0.2, 5))
  }
  // PRUEBAS PROPIAS Huellas(), Jaccard(), Parejas() -----------------------------------------------------------
  // Huellas - 2.5(2)
  test("huellas [+]: La huella de d0 es igual incluso si d0 está adentro del corpus"){
    assert(huella(d0,3) == huellas(c1,3).head)
  }
  
  test("huellas [+]: Dos corpus iguales generan las mismas huellas"){
    assert(huellas(c2,3) == huellas(c2,3))
  }

  val corpusVacio: Corpus = Vector()

  test("huellas [+]: Un corpus vacío da un vector vacío"){
    assert(huellas(corpusVacio,3).isEmpty)
  }

  test("huellas [+]: Cuando hay menos de k palabras, devuelve un vector con sets vacíos"){
    assert(huellas(c2,30).forall(_.isEmpty))
  }

  test("huellas [+]: Con un k menor a 1 los sets contienen un vector vacío"){
    assert(huellas(c1,-2).forall(_.forall(_.isEmpty)))
  }

  // Jaccard - 2.6
  test("jaccard [+]: Dos huellas vacías dan como resultado 0"){
    assert(jaccard(huella(e2,0),huella(e3,0)) == 0)
  }

  test("jaccard [+]: Con una huella vacía, el resultado es 0"){
    assert(jaccard(huella(e2,0),huella(e3,3)) == 0)
  }

  test("jaccard [+]: Las huellas de 2 documentos totalmente distintos da como resultado 0"){
    assert(jaccard(huella(d0,3),huella(e3,3)) == 0)
  }

  test("jaccard [+]: Jaccard de d0 y d1 con k=4 resulta en 0.2"){
    assert(math.abs(jaccard(huella(d0, 4), huella(d1, 4)) - 0.2) < eps)
  }

  test("jaccard [+]: Dos huellas identicas da 1"){
    assert(jaccard(huella(e0,3),huella(e0,3)) == 1)
  }

  // Parejas - 2.7
  test("parejas [+]: i es menor a j"){
    assert(parejas(5).forall { case (a,b) => a < b })
  }

  test("parejas [+]: No hay parejas (i,i)"){
    assert(parejas(5).forall { case (a,b) => a != b })
  }

  test("parejas [+]: n = 1 da un Vector vacío"){
    assert(parejas(1).isEmpty)
  }

  test("parejas [+]: n = 3 da Vector( (0,1), (0,2), (1,2) )"){
    assert(parejas(3) == Vector( (0,1),(0,2),(1,2) ))
  }

  test("parejas [+]: n = 10 da un tamaño de 45 [ 10 (10-1)/2 ]"){
    assert(parejas(10).length == 45)
  }

}
