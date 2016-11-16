package o1.election
import scala.collection.mutable.Buffer

class District(val name: String, val seats: Int, val candidates: Vector[Candidate]) {

  override def toString = this.name + ": " + this.candidates.size + " candidates, " + this.seats + " seats"

  def printCandidates() = {
    for (candidate <- this.candidates) {
      println(candidate)
    }
  }

  def candidatesFrom(party: String) = {
    var fromParty = Buffer[Candidate]()
    for (candidate <- this.candidates) {
      if (candidate.party == party) {
        fromParty += candidate
      }
    }
    fromParty.toVector
  }

  def topCandidate = {
    var topSoFar = this.candidates.head
    for (candidate <- this.candidates.tail) {
      if (candidate > topSoFar) {
        topSoFar = candidate
      }
    }
    topSoFar
  }

  def totalVotes = this.countVotes(this.candidates)

  def totalVotes(party: String) = this.countVotes(this.candidatesFrom(party))

  private def countVotes(candidates: Vector[Candidate]) = {
    var voteCount = 0
    for (candidate <- candidates) {
      voteCount += candidate.votes
    }
    voteCount
  }

  //mapValues, map
  // val candidates = Vector[Candidate]
  
  
  def candidatesByParty: Map[String, Vector[Candidate]] = {
    candidates.groupBy( _.party )
  }
  
  def topCandidatesByParty: Map[String, Candidate] = {
    val kanditPuolueittain = candidatesByParty
    kanditPuolueittain.mapValues( _.maxBy(_.votes))
  }
  
  def votesByParty: Map[String, Int] = {
    val kanditPuolueittain = candidatesByParty
    val äänetPuolueittain = kanditPuolueittain.mapValues( _.map(_.votes))
    äänetPuolueittain.mapValues( _.foldLeft(0)( _ + _ ) )
  }
  
  def rankingsWithinParties: Map[String, Vector[Candidate]] = {
    val kanditPuolueittain = candidatesByParty
    kanditPuolueittain.mapValues( _.sortBy( -_.votes ))
  }
  
  def rankingOfParties: Vector[String] = {
    votesByParty.toVector.sortBy( -_._2 ).map( _._1)
  }
  
  def distributionFigures: Map[Candidate, Double] = {
    def vertailuLuku(kandidaatti: Candidate): Double = (votesByParty(kandidaatti.party)).toDouble / (rankingsWithinParties(kandidaatti.party).indexOf(kandidaatti.name) + 1)  // puolueenäänet / kandinrank
    candidates.map( kandi => kandi -> vertailuLuku(kandi)).toMap
  }
  
  def electedCandidates: Vector[Candidate] = {
    distributionFigures.toVector.sortBy( -_._2 ).map( _._1 ).take(seats)
  }
  
}