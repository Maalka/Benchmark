package com.maalka

object Utils {
  def roundAt(p: Int)(n: Double): Double = {
    val s = math pow(10, p);
    (math round n * s) / s
  }

}
