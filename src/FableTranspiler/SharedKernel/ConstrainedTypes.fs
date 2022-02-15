module ConstrainedTypes

open System


type ConstrainedString =
    /// Create a constrained string using the constructor provided
    /// Return Error if input is null, empty, or length > maxLen
    static member Create(fieldName, ctor, maxLen, str) = 
        if String.IsNullOrEmpty(str) then
            let msg = sprintf "%s must not be null or empty" fieldName 
            Error msg
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
            Error msg 
        else
            Ok (ctor str)

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null, empty, length < minLen or length > maxLen
    static member Create( fieldName, ctor, minLength, maxLen, str ) = 
        if String.IsNullOrEmpty(str) || str.Length < minLength then
            let msg = sprintf "%s must not be null or empty or less then %i" fieldName minLength 
            Error msg
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
            Error msg 
        else
            Ok (ctor str)


type ConstrainedDateTimeOffset =
    static member Create( fieldName, ctor, minDate: DateTime, maxDateOffset: TimeSpan, dateTimeOffset: DateTimeOffset ) =
        let date = dateTimeOffset.Date
        let maxDate = minDate.Add(maxDateOffset)

        if date < minDate then
            let msg = sprintf "%s must not be less then %A" fieldName minDate
            Error msg
        elif date > maxDate then
            let msg = sprintf "%s must not be greater then %A" fieldName maxDate
            Error msg
        else
            Ok (ctor dateTimeOffset)

    static member Create( fieldName, ctor, minDate: DateTime, maxDate: DateTime, dateTimeOffset: DateTimeOffset ) =
        let date = dateTimeOffset.Date

        if date < minDate then
            let msg = sprintf "%s must not be less then %A" fieldName minDate
            Error msg
        elif date > maxDate then
            let msg = sprintf "%s must not be greater then %A" fieldName maxDate
            Error msg
        else
            Ok (ctor dateTimeOffset)

    static member Create( fieldName, ctor, minDate: DateTime, maxDate: DateTime, dateTimeOffset: DateTimeOffset option ) =
        dateTimeOffset
        |> Option.map (fun dateTimeOffset' ->
            let date = dateTimeOffset'.Date

            if date < minDate then
                let msg = sprintf "%s must not be less then %A" fieldName minDate
                Error msg
            elif date > maxDate then
                let msg = sprintf "%s must not be greater then %A" fieldName maxDate
                Error msg
            else
                Ok (ctor dateTimeOffset' |> Some)
        )
        |> Option.defaultValue (Ok None)


/// Create a constrained string using the constructor provided
/// Return Error if input is null, empty, or length > maxLen
let createString fieldName ctor maxLen str = 
    if String.IsNullOrEmpty(str) then
        let msg = sprintf "%s must not be null or empty" fieldName 
        Error msg
    elif str.Length > maxLen then
        let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
        Error msg 
    else
        Ok (ctor str)


/// Create a constrained string using the constructor provided
/// Changes null by empty string
let createText fieldName (ctor: string -> 'a) (str: string) = 
    match str with
    | null ->
        ctor ""
    | v ->
        ctor v


/// Create a optional constrained string using the constructor provided
/// Return None if input is null, empty. 
/// Return error if length > maxLen
/// Return Some if the input is valid
let createStringOption fieldName ctor maxLen str = 
    if String.IsNullOrEmpty(str) then
        Ok None
    elif str.Length > maxLen then
        let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
        Error msg 
    else
        Ok (ctor str |> Some)

/// Create a constrained integer using the constructor provided
/// Return Error if input is less than minVal or more than maxVal
let createInt fieldName ctor minVal i = 
    if i < minVal then
        let msg = sprintf "%s: Must not be less than %i" fieldName minVal
        Error msg
    else
        Ok (ctor i)

/// Create a constrained decimal using the constructor provided
/// Return Error if input is less than minVal or more than maxVal
let createDecimal fieldName ctor minVal maxVal i = 
    if i < minVal then
        let msg = sprintf "%s: Must not be less than %M" fieldName minVal
        Error msg
    elif i > maxVal then
        let msg = sprintf "%s: Must not be greater than %M" fieldName maxVal
        Error msg
    else
        Ok (ctor i)

/// Create a constrained string using the constructor provided
/// Return Error if input is null. empty, or does not match the regex pattern
let createRegex fieldName  ctor pattern str = 
    if String.IsNullOrEmpty(str) then
        let msg = sprintf "%s: Must not be null or empty" fieldName 
        Error msg
    elif System.Text.RegularExpressions.Regex.IsMatch(str,pattern) then
        Ok (ctor str)
    else
        let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
        Error msg 

let inline create fieldName ctor minValue maxValue value =
    if value < minValue  then
        Error (sprintf "%s: Must not be less then %A" fieldName minValue)
    elif value > maxValue then
        Error (sprintf "%s: Must not be greater then %A" fieldName maxValue)
    else
        Ok (ctor value)

let createGuid fieldName ctor (guid: Guid) =
    if guid = Guid.Empty then
        Error (sprintf "%s: Cannot be zero" fieldName)
    else
        Ok (ctor guid)