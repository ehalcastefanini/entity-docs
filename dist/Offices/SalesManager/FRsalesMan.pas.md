<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRsalesMan` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRsalesMan` code unit defines a user interface frame (`TFRAMEsalesMan`) for managing sales manager data. It provides a form-based interface for inputting and editing details such as the sales manager's name, office, email, and login credentials. The frame integrates with backend services to fetch and update data, ensuring seamless interaction with the database.

This frame is part of a larger system for managing sales-related data and is designed to simplify the process of managing sales manager records.

### Technologies Used:
- **Delphi (Object Pascal):** The primary programming language used for the implementation.
- **SOAP Services:** Used for communication with backend services (`TSalesManServiceUtils` and `TOfficeServiceUtils`).
- **Database Components:** Includes `TsDBEdit` and `TDataSource` for binding UI elements to database fields.
- **Custom UI Components:** Includes `TsLabel`, `TsPanel`, and `TsBitBtn` for creating a styled user interface.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - `EDTsalesMan` (Text Input - Database-bound)
  - `EDTname` (Text Input - Database-bound)
  - `FRAMEfindOffice` (Custom Find/Edit Component)
  - `EDTemail` (Text Input - Database-bound)
  - `EDTlogin` (Text Input - Database-bound)
- **Form Actions and Effects:**
  - **Add Button:** Adds a new sales manager record.
  - **Apply Button:** Saves changes to the current record.

---

## 2. Functionality Description:

### User/Software Actions:
- Input and edit sales manager details (name, office, email, login).
- Search and select an office using the `FRAMEfindOffice` component.
- Save changes to the database using the "Apply" button.
- Add new records using the "Add" button.

### Main Components:
- **Labels (`TsLabel`):** Display field names (e.g., "Sales Manager," "Name").
- **Database-bound Text Inputs (`TsDBEdit`):** Allow users to input and edit data.
- **Find/Edit Component (`FRAMEfindOffice`):** Provides a search dialog for selecting an office.
- **Buttons (`TsBitBtn`):** Trigger actions like adding or saving records.

### Pseudo-code for Actions and Events:
- **OnClick event of Add Button:**  
  `if Add button clicked then create new record`
- **OnClick event of Apply Button:**  
  `if Apply button clicked then save changes to database`
- **OnChange event of FRAMEfindOffice:**  
  `if office selected then update officeCode and officeDesc fields`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The frame is initialized with default properties in the `Create` constructor.
   - Backend services (`TSalesManServiceUtils` and `TOfficeServiceUtils`) are configured.
   - The `FRAMEfindOffice` component is set up to allow office selection.
2. **User Interaction:**
   - Users input data into the form fields.
   - Users can search for and select an office using the `FRAMEfindOffice` component.
   - Users click the "Apply" button to save changes or the "Add" button to create a new record.
3. **Functions and File Locations:**
   - `TFRAMEsalesMan.Create` (File: `FRsalesMan.pas`): Initializes the frame.
   - `TFRAMEsalesMan.m_SetFindOffice` (File: `FRsalesMan.pas`): Configures the `FRAMEfindOffice` component.

### Required User Data:
- Sales Manager Name
- Office (selected via `FRAMEfindOffice`)
- Email Address
- Login Credentials

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Button:** Enabled at all times to allow creating new records.
- **Apply Button:** Enabled only when all required fields are filled.

### Available Filters:
- Office selection via `FRAMEfindOffice` with search functionality.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid email format" if the email does not match the expected format.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **Email Field:** Should validate the email format (not explicitly defined in the code).
- **Login Field:** Should ensure uniqueness (not explicitly defined in the code).

---

## 5. Main Functions:

- **`TFRAMEsalesMan.Create`:** Initializes the frame, sets up properties, and configures components.
- **`TFRAMEsalesMan.m_SetFindOffice`:** Configures the `FRAMEfindOffice` component for office selection.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name:** `SalesManServiceUtils`
   - **Endpoint:** Not explicitly defined in the code.
   - **Purpose:** Fetch and update sales manager data.
2. **Service Name:** `OfficeServiceUtils`
   - **Endpoint:** Not explicitly defined in the code.
   - **Purpose:** Fetch office data for the `FRAMEfindOffice` component.

---

## 7. Conditional Fields (Form Logic):

- **Office Field:** Visible and editable only when the `FRAMEfindOffice` component is configured.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient:** For SOAP-based service communication.
- **DBClient:** For database operations.

### Custom Components:
- **`FRAMEfindOffice`:** A custom component for searching and selecting offices.
- **`FRAMEstatusInfo`:** Displays status information.

---

## 9. Fields and Validations Listing:

- **Sales Manager (EDTsalesMan):** Type: String, Required.
- **Name (EDTname):** Type: String, Required.
- **Office (FRAMEfindOffice):** Type: String, Required.
- **Email (EDTemail):** Type: String, Required, Valid email format.
- **Login (EDTlogin):** Type: String, Required.

Mapping of displayed values to database columns is not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [User Inputs Data] --> [User Clicks Apply/Add] --> [Save Data to Database] --> [End]
```

### Sequence Diagram:
```plaintext
User --> FRAMEsalesMan: Input Data
User --> FRAMEfindOffice: Select Office
User --> FRAMEsalesMan: Click Apply
FRAMEsalesMan --> SalesManServiceUtils: Save Data
```

### Code Snippets:
```pascal
procedure TFRAMEsalesMan.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'officeCode';
    EditSettings.FieldNameForDesc := 'officeDesc';
  end;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 781px;">
  <label style="color: #4D4D4D;">Sales Manager:</label>
  <input type="text" placeholder="Enter Sales Manager" />
  <label style="color: #4D4D4D;">Name:</label>
  <input type="text" placeholder="Enter Name" />
  <label style="color: #4D4D4D;">Office:</label>
  <input type="text" placeholder="Select Office" />
  <label style="color: #4D4D4D;">Email:</label>
  <input type="email" placeholder="Enter Email" />
  <label style="color: #4D4D4D;">Login:</label>
  <input type="text" placeholder="Enter Login" />
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization of `FRAMEfindOffice`:** Configures the office selection component.
- **Service Configuration:** Sets up `TSalesManServiceUtils` and `TOfficeServiceUtils` for backend communication.

---

## 12. Conclusion:

The `FRsalesMan` code unit provides a robust and user-friendly interface for managing sales manager data. Its integration with backend services ensures seamless data handling. However, the lack of explicit validation and error handling in the code may require additional implementation.

---

## 13. Short Summary:

The `FRsalesMan` unit defines a form for managing sales manager data, integrating with backend services for data retrieval and updates. It includes fields for name, office, email, and login, with actions for adding and saving records.#### **FRsalesMan.pas**

```
unit FRsalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, kneFRStatusInfo,
  kneFRFindEditSOA, StdCtrls, Mask, DBCtrls, sDBEdit, sFrameAdapter,
  ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, Buttons,
  sBitBtn, sPanel, sLabel;

type
  TFRAMEsalesMan = class(TFRAMEBaseCtrlEditSOA)
    EDTsalesMan: TsDBEdit;
    EDTname: TsDBEdit;
    FRAMEfindOffice: TFRAMEFindEditSOA;
    EDTemail: TsDBEdit;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBLsalesman: TsLabel;
    LBLname: TsLabel;
    LBLoffice: TsLabel;
    LBLemail: TsLabel;
    LBL1: TsLabel;
    EDTlogin: TsDBEdit;
  private
    procedure m_SetFindOffice;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent);  override;
    { Public declarations }
  end;

var
  FRAMEsalesMan: TFRAMEsalesMan;

implementation

uses
  kneUtils, kneTypes,
  SalesManServiceUtils, OfficeServiceUtils;

{$R *.dfm}

constructor TFRAMEsalesMan.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'salesMan';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TSalesManServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindOffice;

  // Atribui��o dos eventos de BeforeFind

  // Atribui��o do evento do After Apply

  // Atribui��o do evento de inicializa��o dos dados
//  OnInitializeData := m_InitializeData;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
end;

procedure TFRAMEsalesMan.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'officeCode';
    EditSettings.FieldNameForDesc := 'officeDesc';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'officeCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('descrip');

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TOfficeServiceUtils.Create(FindDialog);
  end;
end;

end.
```

#### **FRsalesMan.dfm**

```
inherited FRAMEsalesMan: TFRAMEsalesMan
  Width = 781
  object LBLsalesman: TsLabel [0]
    Left = 16
    Top = 16
    Width = 74
    Height = 13
    Caption = 'Sales Manager:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLname: TsLabel [1]
    Left = 16
    Top = 48
    Width = 31
    Height = 13
    Caption = 'Name:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLoffice: TsLabel [2]
    Left = 16
    Top = 80
    Width = 33
    Height = 13
    Caption = 'Office:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLemail: TsLabel [3]
    Left = 16
    Top = 112
    Width = 28
    Height = 13
    Caption = 'Email:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBL1: TsLabel [4]
    Left = 496
    Top = 16
    Width = 29
    Height = 13
    Caption = 'Login:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Width = 781
    TabOrder = 6
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            42020000424D4202000000000000420000002800000010000000100000000100
            1000030000000002000000000000000000000000000000000000007C0000E003
            00001F0000001F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C
            1F7C1F7C1F7C1F7C1F7C1F7C1F7C731DEF10CC18CC18CC00CC181F7C1F7C1F7C
            1F7C1F7C1F7C1F7C1F7C9919CC30C64CC664C664C664C664C630C618CC181F7C
            1F7C1F7C1F7C1F7C9919CC4CC664667166716671667166716671C664C6306708
            1F7C1F7C1F7C1F7CCC4C667166716671D47AFF7FFF7F6C7E66716671C664C630
            CC181F7C1F7CD3306671667166716671737EFF7FFF7F737E667166716671C664
            C6181F7C1F7C8C4D6671667166716671D47AFF7FFF7FD47A6671667166716671
            C64CCC181F7C667166716C7E8C7D6C7ED47AFF7FFF7FD47A6C7E8C7D6C7E6671
            C664CC001F7C66716671FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F6671
            6671CC181F7C66716671FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F6671
            C664EF101F7C6671867DD47AD47AD47AB97BFF7FFF7FB97BD47AD47AD47A6671
            6671EF101F7C8C6566718C7D8C7D6C66D47AFF7FFF7FD47A867D866566716671
            C64C731D1F7C993166716C7E6C7E6C7E5673FF7FFF7FD47A867D867D867D6671
            CC301F7C1F7C1F7C8C4D66716C7E737E397FFF7FFF7FD47A6C7E8C7D6671CC4C
            99191F7C1F7C1F7C1F7C8C4D66716C7ED47A5673D47A6C7E8C656671CC4C9919
            1F7C1F7C1F7C1F7C1F7C1F7C99318C656671867D8C7D66718C65933199191F7C
            1F7C1F7C1F7C}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            42020000424D4202000000000000420000002800000010000000100000000100
            1000030000000002000000000000000000000000000000000000007C0000E003
```
<!-- tabs:end -->

