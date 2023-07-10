package baroque

import gossamer.*
import rudiments.*
import spectacular.*
import quantitative.*
import anticipation.*

val I: Complex[Double] = Complex(0.0, 1.0)

object Complex:
  given show[ComponentType: Show]: Show[Complex[ComponentType]] = complex =>
   t"${complex.real.show} + ${complex.imaginary.show}𝒊"
  
  inline given quantityShow
      [UnitsType <: Measure]
      (using Decimalizer)
      : Show[Complex[Quantity[UnitsType]]] =
    
    new Show[Complex[Quantity[UnitsType]]]:
      def apply(value: Complex[Quantity[UnitsType]]): Text =
        t"${value.real.value} + ${value.imaginary.value}𝒊 ${Quantity.renderUnits(value.real.units)}"
    
  inline given [LeftType, RightType]
      (using multiply: Multiply[LeftType, RightType])
      (using add: Add[multiply.Result, multiply.Result])
      : Multiply[Complex[LeftType], Complex[RightType]] with
    
    type Result = Complex[add.Result]
    
    def apply(left: Complex[LeftType], right: Complex[RightType]) =
      Complex[add.Result](left.real*right.real - left.imaginary*right.imaginary,
          left.real*right.imaginary + left.imaginary*right.real)
  
  inline given [LeftType, RightType]
      (using add: Add[LeftType, RightType])
      : Add[Complex[LeftType], Complex[RightType]] with
    type Result = Complex[add.Result]
    
    def apply
        (left: Complex[LeftType], right: Complex[RightType], subtract: Boolean)
        : Complex[add.Result] =
      if subtract then Complex(left.real + right.real, left.imaginary + right.imaginary)
      else Complex(left.real - right.real, left.imaginary - right.imaginary)
  
case class Complex[+ComponentType](real: ComponentType, imaginary: ComponentType)