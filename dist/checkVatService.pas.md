<!-- tabs:start -->

#### **Documentation**

# Documentation for VAT Check Service Integration Code

## 1. Overview:

### Objective and Problem Solved:
The provided code snippet is designed to integrate with the European Commission's VAT Information Exchange System (VIES) web service. It allows users or applications to validate VAT numbers and retrieve related information. The main objective is to provide a Delphi 7-compatible implementation for interacting with the `checkVat` and `checkVatApprox` operations of the VIES service.

This code solves the problem of validating VAT numbers programmatically, ensuring compliance with EU regulations and enabling businesses to verify the VAT status of their partners or customers.

### Technologies Used:
- **Delphi 7**: The code is specifically adapted for Delphi 7, with modifications to ensure compatibility.
- **SOAP Web Services**: The code interacts with the VIES web service using SOAP.
- **WSDL (Web Services Description Language)**: The service definition is based on the WSDL file provided by the European Commission.
- **Windows API**: Used for internet connectivity checks and other system-level operations.

### Form or Grid Display:
This code does not involve a form or grid display. It is a backend service integration.

---

## 2. Functionality Description:

### Actions:
- Validate a VAT number using the `checkVat` function.
- Perform an approximate VAT number validation using the `checkVatApprox` function.
- Check internet connectivity before making service calls.

### Main Components:
1. **Classes**:
   - `checkVat`: Represents the request parameters for the `checkVat` operation.
   - `checkVatResponse`: Represents the response from the `checkVat` operation.
   - `checkVatApprox`: Represents the request parameters for the `checkVatApprox` operation.
   - `checkVatApproxResponse`: Represents the response from the `checkVatApprox` operation.

2. **Interface**:
   - `checkVatPortType`: Defines the methods for interacting with the VIES service.

3. **Utility Function**:
   - `HasInternetConnectivity`: Checks if the system has internet access.

### Pseudo-code for Actions and Events:
- **Internet Connectivity Check**:
  ```
  if HasInternetConnectivity() then
      proceed with service call
  else
      display error "No internet connection"
  ```

- **VAT Validation**:
  ```
  if checkVat(parameters) called then
      return checkVatResponse
  ```

- **Approximate VAT Validation**:
  ```
  if checkVatApprox(parameters) called then
      return checkVatApproxResponse
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - Classes and interfaces are initialized.
   - Internet connectivity is checked using `HasInternetConnectivity`.

2. **User Interaction**:
   - The user or application provides VAT number details.
   - The appropriate function (`checkVat` or `checkVatApprox`) is called.

3. **Service Call**:
   - The service call is made with a 2-second timeout to prevent the application from hanging.

4. **Response Handling**:
   - The response is processed and returned to the user or application.

### Data Required:
- VAT number (string).
- Country code (string).

---

## 4. Business Rules:

### Actions and Preconditions:
- **`checkVat`**:
  - Preconditions: Valid VAT number and country code must be provided.
  - Action: Validates the VAT number and returns the response.

- **`checkVatApprox`**:
  - Preconditions: Approximate VAT number and country code must be provided.
  - Action: Performs an approximate validation and returns the response.

### Filters:
No filters are explicitly defined in the code.

### Error Messages:
- "No internet connection" if `HasInternetConnectivity` returns false.
- "Service timeout" if the service call exceeds 2 seconds.

### Default Field Values:
Not applicable as the code does not define default values for fields.

### Field Validation and Conditions:
- VAT number: Must be a valid string.
- Country code: Must be a valid string.

---

## 5. Main Functions:

1. **`checkVat`**:
   - Validates a VAT number using the VIES service.
   - Returns a `checkVatResponse` object.

2. **`checkVatApprox`**:
   - Performs an approximate validation of a VAT number.
   - Returns a `checkVatApproxResponse` object.

3. **`HasInternetConnectivity`**:
   - Checks if the system has internet access.

4. **`GetcheckVatPortType`**:
   - Configures and initializes the service interface with a 2-second timeout.

---

## 6. API Service Consumption:

### Service Name:
VIES VAT Validation Service.

### Endpoints:
- `checkVat`
- `checkVatApprox`

### Data Sent:
- `checkVat`: `{ "countryCode": "string", "vatNumber": "string" }`
- `checkVatApprox`: `{ "countryCode": "string", "vatNumber": "string" }`

### Data Received:
- `checkVatResponse`: `{ "valid": "boolean", "name": "string", "address": "string" }`
- `checkVatApproxResponse`: `{ "valid": "boolean", "name": "string", "address": "string" }`

### Purpose:
- Validate VAT numbers and retrieve related information.

### Error Handling:
- Displays "No internet connection" if connectivity is unavailable.
- Displays "Service timeout" if the service call exceeds 2 seconds.

---

## 7. Conditional Fields (Form Logic):

Not applicable as the code does not involve a form.

---

## 8. Dependencies:

### External Libraries:
- **Windows API**: Used for internet connectivity checks.
- **SysUtils, Classes**: Standard Delphi libraries for system utilities and class management.
- **WinInet**: Used for internet-related operations.

### Custom Components:
- None explicitly defined.

---

## 9. Fields and Validations Listing:

### Fields:
- `countryCode` (type: WideString, required).
- `vatNumber` (type: WideString, required).

### Mapping:
- `countryCode` → `countryCode` in the WSDL.
- `vatNumber` → `vatNumber` in the WSDL.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable as the code is backend logic.

### Sequence Diagram:
Not applicable as the code is backend logic.

### Code Snippets:
```delphi
var
  vatService: checkVatPortType;
  response: checkVatResponse;
begin
  if HasInternetConnectivity then
  begin
    vatService := GetcheckVatPortType;
    response := vatService.checkVat(parameters);
    // Process response
  end
  else
    ShowMessage('No internet connection');
end;
```

### Screenshots:
Not applicable as the code does not involve a form.

---

## 11. Important Comments in the Code:

- The code was adapted for Delphi 7 from a newer Delphi version.
- Timeouts were implemented to optimize performance and prevent application hangs.
- Internet connectivity checks were added to ensure reliable service calls.

---

## 12. Conclusion:

The provided code snippet is a robust implementation for integrating with the VIES VAT validation service in Delphi 7. It includes necessary adaptations for compatibility, performance optimizations, and error handling. However, it is limited to Delphi 7 and SOAP-based services, which may not be ideal for modern applications.

---

## 13. Short Summary:

This Delphi 7 code integrates with the VIES VAT validation service, enabling VAT number validation with optimized performance and error handling. It includes internet connectivity checks and a 2-second timeout for reliable service calls.#### **checkVatService.pas**

```
// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://ec.europa.eu/taxation_customs/vies/checkVatService.wsdl
// Encoding : UTF-8
// Version  : 1.0
// (2018/02/21 12:56:21 - 1.33.2.5)
// ************************************************************************ //
// [2018/02/21] #23256
// Este ficheiro foi inicialmente gerado de forma automatica pelo Delphi 7,
// abrindo a opcao
//
//  File > New > Other... > WebServices > WSDL Importer,
//
// e digitando o URL
//
//  http://ec.europa.eu/taxation_customs/vies/checkVatService.wsdl
//
// sendo, em seguida, editado e modificado manualmente para adaptar, para
// Delphi 7, as indicacoes dadas no artigo
//
//  http://www.drbob42.com/examine/examinE2.htm
//
// e tendo por base a informacao do ficheiro 'checkVatService.pas' equivalente
// que e' gerado automaticamente nas versoes mais recentes do Delphi (Delphi XE2+)
//
//  https://github.com/ghquant/Delphi-OmniThreadLibrary/blob/master/examples/checkVat/checkVatService.pas
//
// e alguma informacao adicional, relativamente 'a implementacao de "timeouts"
// maximos para optimizar a performance das chamadas a checkVat() e checkVatApprox()
// (indirectamente, por via da inicializacao feita na funcao GetcheckVatPortType()):
//
//  https://shenoyatwork.blogspot.pt/2006/11/setting-timeouts-in-delphi-soap.html
//
// As adaptacoes para Delphi 7 consistiram, assim, nas seguintes modificacoes:
//
// 1. Foi modificada (corrigida) a definicao de 'matchCode'
//
// 2. Foram (re)definidas as classes
//
//     checkVat = class(TRemotable)
//     checkVatResponse = class(TRemotable)
//     checkVatApprox = class(TRemotable)
//     checkVatApproxResponse = class(TRemotable)
//
//  2.1. Substituindo o tipo 'string' por 'WideString'
//       (em Delphi 7 e' mais seguro indicar explicitamente WideString)
//
//  2.2. Removendo as variaveis membro 'Fxxx_Specified: boolean;', as respectivas
//       funcoes 'function xxx_Specified(Index: Integer): boolean;' e as
//       respectivas referencias 'stored xxx_Specified;' associadas 'as
//       propriedades 'xxx' (e respectivas variaveis membro 'Fxxx')
//
//  2.3. Removendo, nas propriedades 'xxx', os SETters (substituindo-os por
//       declaracoes 'write Fxxx' para escrever directamente nas variaveis
//       membro 'xxx') bem como as referencias
//
//        Index (IS_OPTN)
//        Index (IS_OPTN or IS_NLBL) // <=> Index (IS_OPTN or TSXInteger)
//
// 3. Na definicao do interface
//
//     checkVatPortType = interface(IInvokable)
//
//    foram redefinidos os prototipos das funcoes:
//
//     function  checkVat(const parameters: checkVat): checkVatResponse; stdcall;
//     function  checkVatApprox(const parameters: checkVatApprox): checkVatApproxResponse; stdcall;
//
//  4. Na declaracao 'uses', foram acrescentadas as units
//
//      Windows, SysUtils, Classes, WinInet, FG_Win64utils
//
//     e
//
//   4.1. Implementados os construtores e destrutores definidos nas classes:
//
//         constructor checkVat.Create;
//
//         constructor checkVatResponse.Create;
//         destructor checkVatResponse.Destroy;
//
//         constructor checkVatApprox.Create;
//
//         constructor checkVatApproxResponse.Create;
//         destructor checkVatApproxResponse.Destroy;
//
//   4.2. Implementada uma nova funcao
//
//         HasInternetConnectivity
//
//        que verifica se existe conectividade com o exterior (Internet),
//        optimizando a execucao das funcoes checkVat() e checkVatApprox()! ;)
//
//   4.3. Na funcao
//
//         GetcheckVatPortType
//
//        foram definidos "timeouts" maximos de 2 SEGUNDOS para usar durante
//        a execucao do servico (de modo a nao deixar a aplicacao "pendurada"
```

#### **checkVatService.dfm**

```
dfm file is not needed for this file```
<!-- tabs:end -->

