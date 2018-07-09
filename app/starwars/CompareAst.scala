package starwars

import sangria.ast._

object CompareAst {

  def areEquivalent(schema1: Document, schema2: Document): Boolean = {
    val cleanedSchema1 = cleanSchema(schema1)
    val cleanedSchema2 = cleanSchema(schema2)
    cleanedSchema1 == cleanedSchema2
  }

  private def cleanSchema(schema: Document): Document =
    schema
      .copy(
        definitions = schema.definitions.map(cleanDefinition),
        location = None,
        sourceMapper = None
      )

  private def cleanDefinition(definition: Definition): Definition =
    definition match {
      case otd: ObjectTypeDefinition =>
        otd.copy(
          fields = otd.fields.map(cleanFieldDefinition).sortBy(_.name),
          comments = Vector.empty,
          location = None
        )
      case etd: EnumTypeDefinition =>
        etd.copy(
          values = etd.values.map(cleanEnumValueDefinition).sortBy(_.name),
          comments = Vector.empty,
          location = None
        )
      case x => x
    }

  private def cleanFieldDefinition(
      fieldDefinition: FieldDefinition
  ): FieldDefinition = fieldDefinition.copy(
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
