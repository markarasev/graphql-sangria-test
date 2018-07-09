package starwars

import org.scalactic.Equality
import sangria.ast._

object CompareAst {

  def areEquivalent(schema1: Document, schema2: Document): Boolean =
    DocumentEquality.areEquivalent(schema1, schema2)

  private object DocumentEquality extends Equality[Document] {
    override def areEqual(lhs: Document, b: Any): Boolean = b match {
      case rhs: Document =>
        DefinitionsEquality.areEqual(lhs.definitions, rhs.definitions) &&
          lhs.trailingComments == rhs.trailingComments
      case _ => false
    }
  }

  private object DefinitionsEquality extends Equality[Vector[Definition]] {
    override def areEqual(lhs: Vector[Definition], b: Any): Boolean = b match {
      case rhs: Vector[Definition] =>
        lhs.forall(l => rhs.exists(r => DefinitionEquality.areEqual(l, r))) &&
          lhs.size == rhs.size
      case _ => false
    }
  }

  private object DefinitionEquality extends Equality[Definition] {
    override def areEqual(lhs: Definition, b: Any): Boolean = (lhs, b) match {
      case otds: (ObjectTypeDefinition, ObjectTypeDefinition) =>
        ObjectTypeDefinitionEquality.areEquivalent(otds._1, otds._2)
      case etds: (EnumTypeDefinition, EnumTypeDefinition) =>
        EnumTypeDefinitionEquality.areEquivalent(etds._1, etds._2)
      case _ => false
    }
  }

  private object ObjectTypeDefinitionEquality
      extends Equality[ObjectTypeDefinition] {
    override def areEqual(lhs: ObjectTypeDefinition, b: Any): Boolean =
      b match {
        case rhs: ObjectTypeDefinition =>
          lhs.name == rhs.name &&
            lhs.interfaces == rhs.interfaces &&
            FieldDefinitionsEquality.areEquivalent(lhs.fields, rhs.fields) &&
            DirectivesEquality.areEquivalent(lhs.directives, rhs.directives) &&
            lhs.description == rhs.description &&
            lhs.trailingComments == rhs.trailingComments
        case _ => false
      }
  }

  private object EnumTypeDefinitionEquality
      extends Equality[EnumTypeDefinition] {
    override def areEqual(lhs: EnumTypeDefinition, b: Any): Boolean = b match {
      case rhs: EnumTypeDefinition =>
        lhs.name == rhs.name &&
          EnumValueDefinitionsEquality.areEquivalent(lhs.values, rhs.values) &&
          DirectivesEquality.areEquivalent(lhs.directives, rhs.directives) &&
          lhs.description == rhs.description &&
          lhs.trailingComments == rhs.trailingComments
      case _ => false
    }
  }

  private object FieldDefinitionsEquality
      extends Equality[Vector[FieldDefinition]] {
    override def areEqual(lhs: Vector[FieldDefinition], b: Any): Boolean =
      b match {
        case rhs: Vector[FieldDefinition] =>
          lhs.size == rhs.size &&
            lhs.forall(l =>
              rhs.exists(r => FieldDefinitionEquality.areEqual(l, r)))
        case _ => false
      }
  }

  private object FieldDefinitionEquality extends Equality[FieldDefinition] {
    override def areEqual(a: FieldDefinition, b: Any): Boolean = ???
  }

  private object EnumValueDefinitionsEquality
      extends Equality[EnumValueDefinition] {
    override def areEqual(a: EnumValueDefinition, b: Any): Boolean = ???
  }

  private object DirectivesEquality extends Equality[Vector[Directive]] {
    override def areEqual(lhs: Vector[Directive], b: Any): Boolean = ???
  }

  private def cleanFieldDefinition(
      fieldDefinition: FieldDefinition): FieldDefinition =
    fieldDefinition.copy(
      fieldType = cleanType(fieldDefinition.fieldType),
      arguments = fieldDefinition.arguments.map(cleanInputValueDefinition),
      directives = fieldDefinition.directives.map(cleanDirective),
      comments = Vector.empty,
      location = None
    )

  private def cleanEnumValueDefinition(
      enumValueDefinition: EnumValueDefinition
  ): EnumValueDefinition = enumValueDefinition.copy(
    comments = Vector.empty,
    location = None
  )

  private def cleanInputValueDefinition(
      inputValueDefinition: InputValueDefinition
  ): InputValueDefinition = inputValueDefinition.copy(
    valueType = cleanType(inputValueDefinition.valueType),
    comments = Vector.empty,
    location = None
  )

  private def cleanDirective(directive: Directive): Directive = directive.copy(
    arguments = directive.arguments.map(cleanArgument),
    location = None
  )

  private def cleanArgument(argument: Argument): Argument = argument.copy(
    value = cleanValue(argument.value),
    location = None
  )

  private def cleanType(`type`: Type): Type = `type` match {
    case nt: NamedType                => nt.copy(location = None)
    case nnt @ NotNullType(ofType, _) => nnt.copy(cleanType(ofType), None)
    case lt @ ListType(ofType, _)     => lt.copy(cleanType(ofType), None)
  }

  private def cleanValue(value: Value): Value = value match {
    case sv: StringValue => sv.copy(location = None)
  }

}
