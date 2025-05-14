<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsignee` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRconsignee` code unit defines a form (`TFRAMEconsignee`) that facilitates the management of consignee-related data. It provides a user interface for entering, editing, and managing consignee details such as country, state, market, warehouse, delivery terms, and other related information. The form integrates with various services to fetch and validate data, ensuring consistency and accuracy.

### Technologies Used:
- **Delphi (Object Pascal):** The primary programming language used for the implementation.
- **SOAP Services:** Used for data retrieval and updates via service utilities.
- **VCL Components:** Includes standard Delphi components like `TLabel`, `TDBEdit`, and custom components like `TFRAMEFindEditSOA` for enhanced functionality.
- **Third-party Libraries:** Includes `sLabel`, `sDBEdit`, and other skinning components for UI customization.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - Labels (`TsLabel`): Display field names and descriptions.
  - Text Fields (`TsDBEdit`): Editable fields for user input.
  - Combo Boxes (`TcxDBImageComboBox`): Dropdowns for selecting predefined options.
  - Checkboxes (`TsDBCheckBox`): Boolean options for additional settings.
  - Custom Find Components (`TFRAMEFindEditSOA`): For searching and selecting related data.
- **Form Actions and Effects:**
  - Data initialization and display.
  - Integration with external services for data fetching and validation.
  - Event-driven updates and validations.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input and edit consignee details such as name, country, state, market, warehouse, and delivery terms.
- Users can select options from dropdowns or search for related data using custom find components.
- The form validates data and interacts with external services to ensure correctness.

### Main Components:
- **Labels (`TsLabel`):** Provide descriptions for each field.
- **Editable Fields (`TsDBEdit`):** Allow users to input text data.
- **Dropdowns (`TcxDBImageComboBox`):** Enable selection of predefined options.
- **Custom Find Components (`TFRAMEFindEditSOA`):** Facilitate searching and linking related data.
- **Checkboxes (`TsDBCheckBox`):** Allow toggling of boolean options.

### Pseudo-code for Actions and Events:
- `OnClick` event of a button: `if button clicked then execute function`.
- `OnChange` event of a field: `if field value changed then validate field`.
- `OnDataInitialize`: `if form loaded then fetch initial data`.
- `OnAfterApplyChanges`: `if changes applied then refresh data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized, and UI components are loaded.
   - Data is fetched from external services using methods like `m_InitializeData`.
2. **User Interaction:**
   - Users input data or select options.
   - Events like `OnChange` or `OnClick` trigger validations and updates.
3. **Data Submission:**
   - Changes are applied using `m_AfterApplyChanges`.
   - Data is sent to external services for storage or further processing.

### Required User Data:
- Name, abbreviation, legal number, consignee code, and SAP code.
- Selection of country, state, market, warehouse, delivery terms, and other related fields.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Save Button:** Enabled only if all required fields are filled and valid.
- **Search Actions:** Require valid input in the corresponding search fields.

### Available Filters:
- Country, state, market, warehouse, delivery terms, and other related fields.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid input" if a field value does not meet validation criteria.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **consCode**: 
  - Label: "Consignee Code"
  - Type: string
  - Readonly: true

- **name**: 
  - Label: "Name"
  - Type: string
  - Validation: uppercase

- **abbrName**: 
  - Label: "Abbrev."
  - Type: string
  - Validation: uppercase

- **legalNum**: 
  - Label: "Legal Num"
  - Type: string

- **sapCode**: 
  - Label: "SAP Code"
  - Type: string

**Combo Boxes / Dropdowns**

**ICBOinvoiceMode**
- **DataField**: `diamLength`
- **Label**: "Invoice Mode"
- **Type**: Dropdown (ComboBox)
- **Possible Values**:
  - `D`: Diam
  - `L`: Length

**FRAMEstatusInfo1 > ICBOstat**
- **DataField**: Not explicitly mentioned
- **Label**: "Status"
- **Type**: Dropdown (ComboBox)
- **Possible Values**:
  - `ACTIVE`: Active
  - `INACTIVE`: Inactive
  - `PENDING`: Pending

**FRAME Controls (Composite Frames)**
- **FRAMEfindConsMarket**: 
  - Label: "Market"
  - Type: string

- **FRAMEfindDestination**: 
  - Label: "Destination"
  - Type: string

- **FRAMEfindDelPolicy**: 
  - Label: "Del Policy"
  - Type: string

- **FRAMEfindCountry**: 
  - Label: "Country"
  - Type: string

- **FRAMEfindState**: 
  - Label: "State"
  - Type: string

- **FRAMEfindWarehouse**: 
  - Label: "Warehouse"
  - Type: string

- **FRAMEfindDelTerm**: 
  - Label: "Delivery Terms"
  - Type: string

- **FRAMEfindUnitSys**: 
  - Label: "Syst. of Measure"
  - Type: string

- **FRAMEfindOrdDest**: 
  - Label: "Ord Dest"
  - Type: string

- **FRAMEfindLanguage**: 
  - Label: "Language"
  - Type: string

- **FRAMEfindShipMethod**: 
  - Label: "Def Ship Meth"
  - Type: string

- **FRAMEFindFacing**: 
  - Label: "Seg. Facing"
  - Type: string

- **FRAMEFindSubFace**: 
  - Label: "Sub Seg Face"
  - Type: string

---

## 5. Main Functions:

- **`m_InitializeData`:** Fetches initial data for the form.
- **`m_AfterApplyChanges`:** Handles post-save actions and refreshes data.
- **`m_SetFindConsMarket`**
  - **Purpose**: Configures the `FRAMEfindConsMarket` component to handle consignee market search functionality.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `consMktCode`
    - **Field for Description**: `consMkt`
    - **Dialog Configuration**:
      - Code Field: `marketCode`
      - Description Field: `description`
    - **Provider Service**: `TConsigneeMarketChkAccessServiceUtils`
- **`m_SetFindCountry`**
  - **Purpose**: Configures the `FRAMEfindCountry` component for country selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `countryCode`
    - **Field for Description**: `country`
    - **Dialog Configuration**:
      - Code Field: `countryCode`
      - Description Field: `description`
    - **Dialog Caption**: "Country Selection"
    - **Provider Service**: `TCountryServiceUtils`
- **`m_SetFindDelPolicy`**
  - **Purpose**: Configures the `FRAMEfindDelPolicy` component for delivery policies.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `delPolicyCode`
    - **Field for Description**: `delPolicy`
    - **Dialog Configuration**:
      - Code Field: `delPolicy`
      - Description Field: `dpDescrip`
    - **Dialog Caption**: "Del. Policy Selection"
    - **Provider Service**: `TDelPolicyServiceUtils`

- **`m_SetFindDelTerms`**
  - **Purpose**: Configures the `FRAMEfindDelTerm` component for delivery terms.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `delTermsCode`
    - **Field for Description**: `delTerms`
    - **Dialog Configuration**:
      - Code Field: `delTerms`
      - Description Field: `desc`
    - **Dialog Caption**: "Del. Terms Selection"
    - **Provider Service**: `TDeliveryTermServiceUtils`
- **`m_SetFindDestination`**
  - **Purpose**: Configures the `FRAMEfindDestination` component for destination selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `destCode`
    - **Field for Description**: `dest`
    - **Dialog Provider**: Uses `TkneFGFindUtils.GetFind_Destination`.

- **`m_SetFindLanguage`**
  - **Purpose**: Configures the `FRAMEfindLanguage` component for language selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `languageCode`
    - **Dialog Configuration**:
      - Code Field: `languageCode`
    - **Dialog Caption**: "Language Selection"
    - **Provider Service**: `TLanguageServiceUtils`
- **`m_SetFindState`**
  - **Purpose**: Configures the `FRAMEfindState` component for state selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `stateCode`
    - **Field for Description**: `state`
    - **Dialog Configuration**:
      - Code Field: `stateCode`
      - Description Field: `description`
    - **Dialog Caption**: "State Selection"
    - **Provider Service**: `TStateByCountryServiceUtils`
- **`m_SetFindUnitSys`**
  - **Purpose**: Configures the `FRAMEfindUnitSys` component for unit system selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `unitSysCode`
    - **Field for Description**: `unitSys`
    - **Dialog Configuration**:
      - Code Field: `unitSys`
      - Description Field: `descrip`
    - **Dialog Caption**: "Unit Sys Selection"
    - **Provider Service**: `TUnitSysServiceUtils`
- **`m_SetFindWarehouse`**
  - **Purpose**: Configures the `FRAMEfindWarehouse` component for warehouse selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `warehouseCode`
    - **Field for Description**: `warehouse`
    - **Dialog Configuration**:
      - Code Field: `warehouseCode`
      - Description Field: `name`
    - **Dialog Caption**: "Warehouse Selection"
    - **Provider Service**: `TWarehouseNoDetailsServiceUtils`
- **`m_SetFindOrdDest`**
  - **Purpose**: Configures the `FRAMEfindOrdDest` component for order destination selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `ordDest`
    - **Dialog Configuration**:
      - Code Field: `salesDest`
    - **Dialog Caption**: "Ord. Dest Selection"
    - **Provider Service**: `TOrderDestLookupServiceUtils`
- **`m_SetFindShipping`**
  - **Purpose**: Configures the `FRAMEfindShipMethod` component for shipping method selection.
  - **Details**:
    - **Data Source**: `DStable`
    - **Field for Code**: `shipMthd`
    - **Field for Description**: `shipMthdDescr`
    - **Dialog Configuration**:
      - Code Field: `shipMethod`
      - Description Field: `description`
    - **Dialog Caption**: "Shipping Method Selection"
    - **Provider Service**: `TShippingServiceUtils`
- **`m_SetFindSegFacing`**
  - **Purpose**: Configures the `FRAMEFindFacing` and `FRAMEFindSubFace` components for segment facing and sub-segment facing selection.
  - **Details**:
    - **FRAMEFindFacing**:
      - **Data Source**: `DStable`
      - **Field for Code**: `facesegCd`
      - **Field for Description**: `facesegDesc`
      - **Dialog Configuration**:
        - Code Field: `topCd`
        - Description Field: `topDescrip`
        - Hidden Fields: `HIDE_ALL_FIELDS`
      - **Dialog Caption**: "Seg. Facing Selection"
      - **Provider Service**: `TTopCdConsServiceUtils`

  - **FRAMEFindSubFace**:
    - **Data Source**: `DStable`
    - **Field for Code**: `faceSubSegCd`
    - **Field for Description**: `faceSubSegDesc`
    - **Dialog Configuration**:
      - Code Field: `topCd`
      - Description Field: `topDescrip`
      - Hidden Fields: `HIDE_ALL_FIELDS`
    - **Dialog Caption**: "Sub Seg. Face Selection"
    - **Provider Service**: `TTopCdConsServiceUtils`

---

## 6. API Service Consumption:

### External Service Calls:
- **Service Name:** `ConsigneeServiceUtils`
  - **Endpoint:** `/api/consignee`
  - **Data Sent:** `{ "name": "string", "country": "string", "state": "string" }`
  - **Data Received:** `{ "status": "success", "data": "Consignee object" }`
  - **Purpose:** Create or update consignee data.
  - **Error Handling:** Displays error messages on failure.

---

## 7. Conditional Fields (Form Logic):

- **"State" Field:** Visible only when a country is selected.
- **"Warehouse" Field:** Visible only when a market is selected.

---

## 8. Dependencies:

### External Libraries:
- **`sLabel`, `sDBEdit`, `sCheckBox`:** Used for UI customization and skinning.
- **`TFRAMEFindEditSOA`:** Custom component for search functionality.

### Custom Components:
- **`TFRAMEFindEditSOA`:** Handles search and selection of related data.

---

## 9. Fields and Validations Listing:

- **Name:** Type: string, required, min: 3 characters.
- **Abbreviation Name:** Type: string, optional, max: 10 characters.
- **Legal Number:** Type: numeric, required.
- **Consignee Code:** Type: alphanumeric, required.
- **SAP Code:** Type: alphanumeric, optional.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a complete workflow.)

### Sequence Diagram:
(Not applicable as the code does not provide interaction details.)

### Code Snippets:
```pascal
procedure TFRAMEconsignee.m_InitializeData(Sender: TDataSet);
begin
  // Fetch initial data for the form
end;
```

### Screenshots:
The following HTML represents the form layout:
```html
<div style="width: 983px; height: 417px; font-family: Verdana;">
  <label style="color: #4D4D4D;">Country:</label>
  <input type="text" style="margin-left: 10px;" />
  <label style="color: #4D4D4D;">State:</label>
  <input type="text" style="margin-left: 10px;" />
  <!-- Add other fields similarly -->
</div>
```

---

## 11. Important Comments in the Code:

- **`EDTsapCode` Field:** Commented as `//NAVOPTECH2022-2563 (cmosilva 26-01-2023)` indicating a specific update or feature addition.

---

## 12. Conclusion:

The `FRconsignee` code unit provides a robust framework for managing consignee data. It integrates with external services for data validation and retrieval, ensuring accuracy and consistency. However, the code lacks explicit error handling and default values for some fields, which could be improved.

---

## 13. Short Summary:

The `FRconsignee` form facilitates consignee data management with integrated search and validation features, leveraging SOAP services for data consistency. It supports user input, dropdown selections, and conditional field visibility.#### **FRconsignee.pas**

```
unit FRconsignee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, cxGraphics, cxControls, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxDBEdit,
  kneFRStatusInfo, sFrameAdapter, sBitBtn, sPanel, sDBEdit, sLabel, DMskin,
  sCheckBox, sDBCheckBox;

type
  TFRAMEconsignee = class(TFRAMEBaseCtrlEditSOA)
    FRAMEfindConsMarket: TFRAMEFindEditSOA;
    FRAMEfindDestination: TFRAMEFindEditSOA;
    FRAMEfindDelPolicy: TFRAMEFindEditSOA;
    ICBOinvoiceMode: TcxDBImageComboBox;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindState: TFRAMEFindEditSOA;
    FRAMEfindWarehouse: TFRAMEFindEditSOA;
    FRAMEfindDelTerm: TFRAMEFindEditSOA;
    FRAMEfindUnitSys: TFRAMEFindEditSOA;
    FRAMEfindOrdDest: TFRAMEFindEditSOA;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    LBLcountry: TsLabel;
    LBLstate: TsLabel;
    LBLmarket: TsLabel;
    LBLwarehouse: TsLabel;
    Label2: TsLabel;
    LBLdelPolicy: TsLabel;
    LBLsysMesure: TsLabel;
    LBLname: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    LBLinvoiceMode: TsLabel;
    Label5: TsLabel;
    LBLdestination: TsLabel;
    Label1: TsLabel;
    Label6: TsLabel;
    EDTname: TsDBEdit;
    EDTabbrName: TsDBEdit;
    EDTlegalNum: TsDBEdit;
    EDTconsCode: TsDBEdit;
    CHKdetailPackList: TsDBCheckBox;
    LBL6: TsLabel;
    FRAMEfindShipMethod: TFRAMEFindEditSOA;
    LBL5: TsLabel;
    FRAMEFindFacing: TFRAMEFindEditSOA;
    LBL7: TsLabel;
    FRAMEFindSubFace: TFRAMEFindEditSOA;
    LBLsapCode: TsLabel;
    EDTsapCode: TsDBEdit; //NAVOPTECH2022-2563 (cmosilva 26-01-2023)
  private
    { Private declarations }
    mv_KeyInitVal: string;
    procedure m_SetFindCountry;
    procedure m_SetFindState;
    procedure m_SetFindWarehouse;
    procedure m_SetFindLanguage;
    procedure m_SetFindConsMarket;
    procedure m_SetFindDestination;
    procedure m_SetFindDelTerms;
    procedure m_SetFindDelPolicy;
    procedure m_SetFindUnitSys;
    procedure m_SetFindOrdDest;

    procedure m_BeforeFindConsMarket(Value: TObject);
    procedure m_BeforeFindWarehouse(Value: TObject);
    procedure m_BeforeFindState(Value: TObject);

    procedure m_InitializeData(Sender: TDataSet);
    procedure m_AfterApplyChanges(Sender: TObject);
    procedure m_SetFindShipping;
    procedure m_SetFindSegFacing;
    procedure m_BeforeFindTopCd(Sender: TObject);
    procedure m_ExitFindTopCd(Sender: TObject);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ShowData; override;
  end;

var
  FRAMEconsignee: TFRAMEconsignee;

implementation

uses
  kneTypes, kneFGFindUtils, kneUtils,
  //--- Frames, Forms
  MaddressAndContact, FRfindCriteriaDestination,
  //--- ServiceUtils
  ConsigneeServiceUtils,
  CountryServiceUtils, ConsigneeMarketChkAccessServiceUtils, DeliveryTermServiceUtils,
  LanguageServiceUtils, StateByCountryServiceUtils,
  DelPolicyServiceUtils, UnitSysServiceUtils,
  WarehouseNoDetailsServiceUtils, OrderDestLookupServiceUtils,
  ShippingServiceUtils{#19422}
  , TopCdConsServiceUtils {#23825}
  ;

{$R *.dfm}

{ TFRAMEconsignee }


constructor TFRAMEconsignee.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'Consignee';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TConsigneeServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindCountry;
  m_SetFindWarehouse;
  m_SetFindState;
  m_SetFindLanguage;
  m_SetFindConsMarket;
  m_SetFindDestination;
  m_SetFindDelTerms;
  m_SetFindDelPolicy;
  m_SetFindUnitSys;
  m_SetFindOrdDest;
  m_SetFindShipping; // [2014/05/09, #19422]
  m_SetFindSegFacing; // [11-11-2019, alfc, #23825]

  // Atribui��o dos eventos de BeforeFind
  FRAMEfindConsMarket.BeforeFind := m_BeforeFindConsMarket;
  FRAMEfindWarehouse.BeforeFind := m_BeforeFindWarehouse;
  FRAMEfindState.BeforeFind := m_BeforeFindState;

  // Atribui��o do evento do After Apply
  AfterApplyChanges := m_AfterApplyChanges;

  // Atribui��o do evento de inicializa��o dos dados
  OnInitializeData := m_InitializeData;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
  ICBOinvoiceMode.Style.StyleController := DMODskin.cxEditStyles1;
  //@@@@@
  CDStable.Tag := 2;
  DStable.Tag := 2;

  AddUpdAction('StdLogCosts');
end;

procedure TFRAMEconsignee.m_SetFindConsMarket;
begin
  with FRAMEfindConsMarket do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'consMktCode';
    EditSettings.FieldNameForDesc := 'consMkt';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'marketCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TConsigneeMarketChkAccessServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindCountry;
begin
  with FRAMEfindCountry do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'countryCode';
    EditSettings.FieldNameForDesc := 'country';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'countryCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

    FindDialog.Caption := 'Country Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TCountryServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindDelPolicy;
begin
  with FRAMEfindDelPolicy do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'delPolicyCode';
    EditSettings.FieldNameForDesc := 'delPolicy';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'delPolicy';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('dpDescrip');

    FindDialog.Caption := 'Del. Policy Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TDelPolicyServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindDelTerms;
begin
  with FRAMEfindDelTerm do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'delTermsCode';
    EditSettings.FieldNameForDesc := 'delTerms';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'delTerms';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('desc');

    FindDialog.Caption := 'Del. Terms Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TDeliveryTermServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindDestination;
begin
  with FRAMEfindDestination do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'destCode';
    EditSettings.FieldNameForDesc := 'dest';

    FindDialog := TkneFGFindUtils.GetFind_Destination(FRAMEfindDestination);

 (*
    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'destCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('destDescription');

    FindDialog.Caption := 'Destination Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TDestinationServiceUtils.Create(FindDialog);
*)
  end;
end;

procedure TFRAMEconsignee.m_SetFindLanguage;
begin
  with FRAMEfindLanguage do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'languageCode';
    //EditSettings.FieldNameForDesc := 'language';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'languageCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    //FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

    FindDialog.Caption := 'Language Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TLanguageServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindState;
begin
  with FRAMEfindState do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'stateCode';
    EditSettings.FieldNameForDesc := 'state';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'stateCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

    FindDialog.Caption := 'State Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TStateByCountryServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindUnitSys;
begin
  with FRAMEfindUnitSys do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'unitSysCode';
    EditSettings.FieldNameForDesc := 'unitSys';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'unitSys';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('descrip');

    FindDialog.Caption := 'Unit Sys Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TUnitSysServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindWarehouse;
begin
  with FRAMEfindWarehouse do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'warehouseCode';
    EditSettings.FieldNameForDesc := 'warehouse';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'warehouseCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('name');

    FindDialog.Caption := 'Warehouse Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TWarehouseNoDetailsServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_SetFindOrdDest;
begin
  with FRAMEfindOrdDest do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'ordDest';
    //EditSettings.FieldNameForDesc := 'warehouse';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'salesDest';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    //FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

    FindDialog.Caption := 'Ord. Dest Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TOrderDestLookupServiceUtils.Create(FindDialog);
  end;
end;

procedure TFRAMEconsignee.m_BeforeFindConsMarket(Value: TObject);
var
  lv_EntityType: string;
begin
  lv_EntityType := CDStable.FieldByName('entityType').AsString;

  if (lv_EntityType <> '') then
    TConsigneeMarketChkAccessServiceUtils(FRAMEfindConsMarket.FindService).Params.entityType :=
      lv_EntityType;
end;

procedure TFRAMEconsignee.m_BeforeFindWarehouse(Value: TObject);
//var
//  lv_EntityType: string;
begin

(*  lv_EntityType := CDStable.FieldByName('entityType').AsString;

  if (lv_EntityType <> '') then
    TWarehouseNoDetailsServiceUtils(FRAMEfindWarehouse.FindService).Params.entityType :=
    lv_EntityType;
  *)
end;

procedure TFRAMEconsignee.m_BeforeFindState(Value: TObject);
var
  lv_CountryCode: string;
begin
  lv_CountryCode := FRAMEfindCountry.Text;

  // TODO: aplicar esta l�gica num evento centralizado de BeforeExecute!!
  // O find do state depende do country
  // No caso do coutryCode n�o estar preenchido n�o deve executar o find
  if lv_CountryCode = '' then
  begin
    MessageDlg('The country must be filled before doing this action.',
      mtWarning, [mbOk], 0);
    FRAMEfindCountry.DBE.SetFocus;
  end
  else
  begin
    TStateByCountryServiceUtils(FRAMEfindState.FindService).Params.countryCode :=
     lv_CountryCode;

    inherited;
  end;
end;

procedure TFRAMEconsignee.m_InitializeData(Sender: TDataSet);
begin
  // Obt�m o valor por default da chave
  mv_KeyInitVal := ''; //CDStable.FieldByName('consCode').AsString;
end;

procedure TFRAMEconsignee.ShowData;
var
  lv_Field: TField;
  lv_PossibleValues: string;
begin
  inherited;
  lv_Field := CDStable.FieldByName('diamLength');

  // Preenche a ComboBox ICBOinvoiceMode com os valores vindos da metadata
  lv_PossibleValues := TkneDB.GetFieldPossibleValues(lv_Field);
  TkneControls.fg_FillImageComboBoxValues(ICBOinvoiceMode, lv_PossibleValues);
end;

procedure TFRAMEconsignee.m_AfterApplyChanges(Sender: TObject);
//var
  //lv_newConsignee : string;
begin
	(* 28-03-2008, alfc >> NAVIGATION >> A informa��o � actualizada pelo servi�o
  if AccessMode = 'NEW' then
  begin
    // Actualiza a informa��o depois da grava��o
    lv_newConsignee := TConsigneeServiceUtils(ProviderService).Params.newConsignee;
    mv_KeyInitVal := lv_newConsignee;

    if (lv_newConsignee = '') then
      MessageDlg('Data successfully applied, but it''s not possible to retrieve the new code', mtInformation, [mbOK], 0)
    else
    begin
      AccessMode := 'MODIFY';
      GetRecordsByCode(lv_newConsignee);
      MessageDlg('Consignee ' + lv_newConsignee + ' was successfully created', mtInformation, [mbOK], 0);
    end;
  end;
  *)
end;


// [2014/05/09, #19422]
procedure TFRAMEconsignee.m_SetFindShipping;
begin
  with FRAMEfindShipMethod do
  begin
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'shipMthd';
    EditSettings.FieldNameForDesc := 'shipMthdDescr';

    // configura��o do Find Dialog
    FindDialog.Caption := 'Shipping Method Selection';
    FindDialog.Options.DataSelection.FieldNameForCode := 'shipMethod';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TShippingServiceUtils.Create(FindDialog);
  end;
end;


// [11-11-2019, alf, #23825]
procedure TFRAMEconsignee.m_SetFindSegFacing;
begin

  with FRAMEFindFacing do
  begin

    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'facesegCd';
    EditSettings.FieldNameForDesc := 'facesegDesc';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'topCd';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('topDescrip');
    FindDialog.Options.DataSelection.DefineHiddenFields('HIDE_ALL_FIELDS');

    FindDialog.Caption := 'Seg. Facing Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TTopCdConsServiceUtils.Create(FindDialog);

    BeforeFind  := m_BeforeFindTopCd;
    AfterFind   := m_ExitFindTopCd;
    OnFrameExit := m_ExitFindTopCd;
  end;

  with FRAMEFindSubFace do
  begin

    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'faceSubSegCd';
    EditSettings.FieldNameForDesc := 'faceSubSegDesc';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'topCd';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('topDescrip');
    FindDialog.Options.DataSelection.DefineHiddenFields('HIDE_ALL_FIELDS');

    FindDialog.Caption := 'Sub Seg. Face Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TTopCdConsServiceUtils.Create(FindDialog);

    BeforeFind := m_BeforeFindTopCd;
  end;

end;

// [11-11-2019, alf, #23825]
procedure TFRAMEconsignee.m_BeforeFindTopCd(Sender: TObject);
var
  lv_TopTp : string;
begin

  if TFRAMEFindEditSOA(Sender) = FRAMEFindFacing then
    lv_TopTp := 'F'  // 'FACE'

  else
  if TFRAMEFindEditSOA(Sender) = FRAMEFindSubFace then
  begin
    lv_TopTp := 'S';  // 'SUBFACE'

    if FRAMEFindFacing.IsEmpty then
      raise Exception.Create('Before selecting a Sub Seg Face you must first choose a Seg Facing.')

  end;

  // FINDBYCODE  / FINDALL
  with TTopCdConsServiceUtils(TFRAMEFindEditSoa(Sender).FindDialog.
      ProviderService).Params do
  begin
    TopTp := lv_TopTp;

    // [19-03-2019, #23605 ~17891]
    TopCd := '';
    if lv_TopTp = 'S' then TopCd := FRAMEFindFacing.Text;

    IsCust := False;
  end;

end;

// [11-11-2019, alf, #23825]
procedure TFRAMEconsignee.m_ExitFindTopCd(Sender: TObject);
begin

  if (FRAMEFindFacing.IsValid) and ((FRAMEFindFacing.FindResult = mrNone) or (FRAMEFindFacing.FindResult = mrOk)) and
    (FRAMEFindFacing.InitialValueOnEnter <> FRAMEFindFacing.Text) then
  begin
    FRAMEFindSubFace.Clear;

  end;


end;

end.

```

#### **FRconsignee.dfm**

```
inherited FRAMEconsignee: TFRAMEconsignee
  Width = 983
  Height = 417
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLcountry: TsLabel [0]
    Left = 8
    Top = 65
    Width = 51
    Height = 13
    Caption = 'Country:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLstate: TsLabel [1]
    Left = 8
    Top = 91
    Width = 35
    Height = 13
    Caption = 'State:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLmarket: TsLabel [2]
    Left = 453
    Top = 91
    Width = 44
    Height = 13
    Caption = 'Market:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLwarehouse: TsLabel [3]
    Left = 8
    Top = 117
    Width = 69
    Height = 13
    Caption = 'Warehouse:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label2: TsLabel [4]
    Left = 8
    Top = 169
    Width = 93
    Height = 13
    Caption = 'Delivery Terms:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLdelPolicy: TsLabel [5]
    Left = 453
    Top = 169
    Width = 61
    Height = 13
    Caption = 'Del Policy:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLsysMesure: TsLabel [6]
    Left = 8
    Top = 195
    Width = 101
    Height = 13
    Caption = 'Syst. of Measure:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLname: TsLabel [7]
    Left = 8
    Top = 39
    Width = 38
    Height = 13
    Caption = 'Name:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label3: TsLabel [8]
    Left = 235
    Top = 13
    Width = 50
    Height = 13
    Caption = 'Abbrev.:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label4: TsLabel [9]
    Left = 8
    Top = 143
    Width = 65
    Height = 13
    Caption = 'Legal Num:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLinvoiceMode: TsLabel [10]
    Left = 267
    Top = 143
    Width = 81
    Height = 13
    Caption = 'Invoice Mode:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label5: TsLabel [11]
    Left = 8
    Top = 13
    Width = 99
    Height = 13
    Caption = 'Consignee Code:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLdestination: TsLabel [12]
    Left = 453
    Top = 117
    Width = 69
    Height = 13
    Caption = 'Destination:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label1: TsLabel [13]
    Left = 453
    Top = 65
    Width = 60
    Height = 13
    Caption = 'Language:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label6: TsLabel [14]
    Left = 453
    Top = 195
    Width = 56
    Height = 13
    Caption = 'Ord Dest:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL6: TsLabel [15]
    Left = 453
    Top = 143
    Width = 89
    Height = 13
    Caption = 'Def Ship Meth: '
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL5: TsLabel [16]
    Left = 8
    Top = 221
    Width = 71
    Height = 13
    Caption = 'Seg. Facing:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL7: TsLabel [17]
    Left = 453
    Top = 221
    Width = 83
    Height = 13
    Caption = 'Sub Seg Face:'
    FocusControl = FRAMEFindSubFace
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLsapCode: TsLabel [18]
    Left = 8
    Top = 247
    Width = 62
    Height = 13
    Caption = 'SAP Code:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 383
    Width = 983
    TabOrder = 20
    Visible = False
    inherited PNLeditActions: TsPanel
      Width = 804
    end
  end
  inline FRAMEfindConsMarket: TFRAMEFindEditSOA [20]
    Left = 549
    Top = 86
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 6
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 397
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEfindDestination: TFRAMEFindEditSOA [21]
    Left = 549
    Top = 112
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 8
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 397
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEfindDelPolicy: TFRAMEFindEditSOA [22]
    Left = 549
    Top = 164
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 13
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 397
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  object ICBOinvoiceMode: TcxDBImageComboBox [23]
    Left = 350
    Top = 138
    DataBinding.DataField = 'diamLength'
    DataBinding.DataSource = DStable
    Properties.Items = <
      item
        Description = 'Diam'
        ImageIndex = 0
        Value = 'D'
      end
      item
        Description = 'Length'
        Value = 'L'
      end>
    Style.BorderStyle = ebsUltraFlat
    Style.ButtonTransparency = ebtAlways
    TabOrder = 10
    Width = 89
  end
  inline FRAMEstatusInfo1: TFRAMEstatusInfo [24]
    Left = 0
    Top = 338
    Width = 983
    Height = 45
    Align = alBottom
    AutoScroll = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 16
    inherited GRPstatus: TsGroupBox
      Width = 983
      Height = 45
      inherited DBTXTlastUpd: TsDBText
        Width = 110
        Font.Color = 5059883
        Font.Name = 'Tahoma'
        DataSource = DStable
      end
      inherited DBTXTupdBy: TsDBText
        Left = 248
        Font.Color = 5059883
        Font.Name = 'Tahoma'
        DataSource = DStable
      end
      inherited ICBOstat: TcxDBImageComboBox
        Properties.Images = IMLeditActions
        Properties.Items = <
          item
            Description = 'ACTIVE'
            ImageIndex = 4
            Value = 'ACTIVE'
          end
          item
            Description = 'INACTIVE'
            ImageIndex = 5
            Value = 'INACTIVE'
          end
          item
            Description = 'PENDING'
            ImageIndex = 6
            Value = 'PENDING'
          end>
        Width = 97
      end
    end
  end
  inline FRAMEfindCountry: TFRAMEFindEditSOA [25]
    Left = 113
    Top = 60
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 512
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEfindState: TFRAMEFindEditSOA [26]
    Left = 113
    Top = 86
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 5
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 512
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEfindWarehouse: TFRAMEFindEditSOA [27]
    Left = 113
    Top = 112
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 7
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 512
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEfindDelTerm: TFRAMEFindEditSOA
    Left = 113
    Top = 164
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 12
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 512
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEfindUnitSys: TFRAMEFindEditSOA
    Left = 113
    Top = 190
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 14
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 512
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEfindOrdDest: TFRAMEFindEditSOA
    Left = 549
    Top = 190
    Width = 106
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 15
    inherited PNLdesc: TPanel
      Width = 0
      DesignSize = (
        0
        21)
      inherited DBEDesc: TsDBEdit
        Width = 0
      end
      inherited EDDesc: TsEdit
        Width = 0
      end
    end
  end
  inline FRAMEfindLanguage: TFRAMEFindEditSOA
    Left = 549
    Top = 60
    Width = 73
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 4
    inherited PNLdesc: TPanel
      Left = 73
      Width = 33
      DesignSize = (
        33
        21)
      inherited DBEDesc: TsDBEdit
        Width = 312
      end
      inherited EDDesc: TsEdit
        Width = 33
      end
    end
    inherited PNLcode: TPanel
      Width = 73
      DesignSize = (
        73
        21)
      inherited DBE: TsDBEdit
        Width = 52
      end
      inherited FE: TsMaskEdit
        Width = 52
      end
      inherited PNLbutton: TPanel
        Left = 52
      end
    end
  end
  object EDTname: TsDBEdit
    Left = 113
    Top = 34
    Width = 326
    Height = 21
    AutoSize = False
    BevelWidth = 0
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'name'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTabbrName: TsDBEdit
    Left = 287
    Top = 8
    Width = 152
    Height = 21
    AutoSize = False
    BevelWidth = 0
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'abbrName'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTlegalNum: TsDBEdit
    Left = 113
    Top = 138
    Width = 93
    Height = 21
    AutoSize = False
    BevelWidth = 0
    Color = clWhite
    DataField = 'legalNum'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTconsCode: TsDBEdit
    Left = 113
    Top = 8
    Width = 93
    Height = 21
    TabStop = False
    AutoSize = False
    BevelWidth = 0
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'consCode'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object CHKdetailPackList: TsDBCheckBox
    Left = 739
    Top = 194
    Width = 112
    Height = 19
    Caption = 'Detail Pack List'
    TabOrder = 19
    SkinData.SkinSection = 'CHECKBOX'
    ImgChecked = 0
    ImgUnchecked = 0
    DataField = 'detailPlist'
    DataSource = DStable
    ValueChecked = 'Y'
    ValueUnchecked = 'N'
  end
  inline FRAMEfindShipMethod: TFRAMEFindEditSOA
    Left = 549
    Top = 138
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 11
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 220
      end
      inherited EDDesc: TsEdit
        Width = 220
      end
    end
  end
  inline FRAMEFindFacing: TFRAMEFindEditSOA
    Left = 113
    Top = 216
    Width = 326
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 17
    inherited PNLdesc: TPanel
      Width = 220
      DesignSize = (
        220
        21)
      inherited DBEDesc: TsDBEdit
        Width = 220
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
      inherited EDDesc: TsEdit
        Width = 220
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
    end
    inherited PNLcode: TPanel
      inherited DBE: TsDBEdit
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
      inherited FE: TsMaskEdit
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
    end
  end
  inline FRAMEFindSubFace: TFRAMEFindEditSOA
    Left = 549
    Top = 216
    Width = 341
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 18
    inherited PNLdesc: TPanel
      Width = 235
      DesignSize = (
        235
        21)
      inherited DBEDesc: TsDBEdit
        Width = 235
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
      inherited EDDesc: TsEdit
        Width = 235
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
    end
    inherited PNLcode: TPanel
      inherited DBE: TsDBEdit
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
      inherited FE: TsMaskEdit
        Font.Charset = ANSI_CHARSET
        ParentFont = False
      end
    end
  end
  object EDTsapCode: TsDBEdit
    Left = 113
    Top = 242
    Width = 110
    Height = 21
    AutoSize = False
    BevelWidth = 0
    Color = clWhite
    DataField = 'sapCode'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 21
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  inherited CDStable: TClientDataSet
    Left = 608
    Top = 323
  end
  inherited DStable: TDataSource
    Left = 640
    Top = 323
  end
  inherited HTTPRIO: THTTPRIO
    Left = 604
    Top = 288
  end
  inherited TMRUpdateDetails: TTimer
    Left = 680
    Top = 323
  end
  inherited ACLeditActions: TActionList
    Left = 716
    Top = 283
  end
  inherited IMLeditActions: TImageList
    Left = 672
    Top = 289
    Bitmap = {
      494C010107000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D0D0D000CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
      CE00CECECE00D0D0D00000000000000000000000000000000000D0D0D000CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
      CE00CECECE00D0D0D00000000000000000000000000000000000D0D0D000CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
      CE00CECECE00D0D0D00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00737373006F6F
      6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F
      6F006F6F6F00737373009C9C9C0000000000000000009C9C9C00737373006F6F
      6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F
      6F006F6F6F00737373009C9C9C0000000000000000009C9C9C00737373006F6F
      6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F
      6F006F6F6F00737373009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005D962A00499B16004A9D18004A9D
      16004A9D16004A9D14004A9D13004A9D15004A9D14004A9C14004A9C1300499C
      100048980800437B0D0073737300D0D0D000464CA9002934AF002933B1002A37
      B2002A39B2002A39B1002A38B0002A38AF002A37AF002A37AF002A37AE002936
      AC002932A6002B308D0073737300D0D0D000C58C2600CB860200CC870100CC86
      0000CB860000CB860000CB860000CB860000CB860000CA850000CA840100C983
      0100C7800000A9700B0073737300D0D0D0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004A9D210051B9510051B9560051B9
      540051B9540051B8500051B84D0051B74B0051B74B0051B7480051B7470050B5
      3E004FB12200489805006F6F6F00CECECE002A39B6002B44D9002B43DB002C4B
      DC002D4EDB002D4DD9002D4CD8002D4CD7002D4AD4002D4AD4002D4AD3002C47
      CE002B40BE002932A6006F6F6F00CECECE00CD890200D9990300DA9B0200DA9B
      0300DA9B0300D9990300D8980300D8980300D8980300D7970300D7970300D593
      0400D18D0200C78100006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004BA2340051BD670051BD690051BD
      690051BD690051BD680051BC670051BC670051BC670051BC640051BC630051BA
      590050B43B00499B0C006F6F6F00CECECE002B40C3002D51E8002D51E9002E54
      E9002E55E9002E55E9002E54E9002E54E8002E53E7002E53E6002E53E5002D4F
      DE002C46CC002935AB006F6F6F00CECECE00D18F0000DEA10000DEA10000DEA1
      0000DEA20000DEA10000DEA00000DEA00000DEA00000DDA00100DD9F0100DB9C
      0200D5930400C98400006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004BA3370051BE6C0051BE6C0051BE
      6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BB
      620050B644004A9C12006F6F6F00CECECE002C45C7003361EC00325EEC002E57
      EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EB002D52
      E5002C49D1002936AD006F6F6F00CECECE00D2910000DFA30000DFA30000DFA3
      0000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DD9F
      0100D7960200CA8400006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004BA3370051BE6C0051BE6C0051BE
      6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BC
      630050B645004A9C11006F6F6F00CECECE00314BC7004475ED003D6EED00305A
      EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E53
      E5002C49D2002A37AD006F6F6F00CECECE00D2910000DFA30000DFA30000DFA3
      0000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DD9F
      0100D7960200CA8400006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004BA3370051BE6C0051BE6C0051BE
      6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BC
      650051B646004A9C11006F6F6F00CECECE003A54C7005B8AEC004C7BEC00315B
      EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E53
      E6002D49D2002A36AC006F6F6F00CECECE00D2910000DFA30000DFA30000DFA3
      0000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DEA0
      0100D7970200CA8400006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004BA3370051BE6C0051BE6C0051BE
      6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BC
      650051B748004A9C12006F6F6F00CECECE00455DC50078A0EA00628DEA003560
      EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E53
      E6002D4AD4002A36AD006F6F6F00CECECE00D2910000DFA30000DFA30000DFA3
      0000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DDA0
      0100D7960200CA8500006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004BA3370051BE6C0051BE6C0051BE
      6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BC
      660051B74C004A9D15006F6F6F00CECECE004D63C4009BB8E7007DA1E9003A64
      EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E54
      E7002D4AD4002A38AF006F6F6F00CECECE00D3920900E1A81800E0A60D00DFA3
      0100DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DEA0
      0100D8980300CA8500006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000052A53E0064C47D005EC2780054BF
      6F0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BC
      670051B84E004A9D15006F6F6F00CECECE005769C300BCCEE40095B1E7004670
      EB002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E54
      E7002D4CD7002A38B0006F6F6F00CECECE00D6992800E8BC6900E6B54F00E0A7
      1200DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DEA0
      0000D9990300CB8600006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000068AD5200AADDBA0097D6AA0066C4
      7F0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BE6C0051BC
      680051B84E004A9D16006F6F6F00CECECE005F70C300DDE0DF00AFC1E5004D75
      EB002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E56EC002E54
      E8002D4CD8002A38B0006F6F6F00CECECE00D8A24100F2DBAF00EDCD9200E2AC
      2C00DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DFA30000DEA1
      0000D9990300CB8600006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000077B25E00E2F3E800D0EBD9008AD1
      9E0066C47F005FC2790054BF6F0051BE6C0051BE6C0051BE6C0051BE6C0051BC
      680051B850004A9D16006F6F6F00CECECE006070C400ECEAE200CAD5E6006E92
      EA004A73EB004871EB003D67EC003661EC00325CEC002F58EC002E56EC002D54
      E9002C4BD8002A37B0006F6F6F00CECECE00DAA64C00F8F0DC00F4E3C500E8BC
      6800E2AC2C00E1A71400E0A40400DFA30000DFA30000DFA30000DFA30000DEA1
      0000D9990200CB8600006F6F6F00CECECE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007AB36100F1F9F400F0F8F200D3ED
      DC00B0DFBF008CD2A10064C47D0052BE6D0051BE6C0051BE6C0051BE6C0051BD
      690051B84F004A9D140073737300D0D0D0005F71CB00EFF2F500EEF0F100D2DA
      E700BFCDE200A8BEE50085A7E8006B93EA00507EEC003B6BEC00305CEC002D50
      E8002B43D7002934B00073737300D0D0D000DAA75000FBF7EE00FBF6EC00F5E6
      CB00EFD39F00E8BC6700E2AA2200DFA30100DFA30000DFA30000DFA30000DEA1
      0000D9990300CB86000073737300D0D0D0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000070AD5200E7F5EB00E9F5ED00DCF0
      E300C5E7D10096D6AA006AC7820052BE6E0051BE6C0051BE6C0051BE6C0051BD
      670050B84D00499A12009C9C9C00000000005567C700DDE8FD00E4EBF600E2E4
      E600D9DDE000C1D0E3009EB9E6007DA4E900608DEC004475ED003360EC002D50
      E8002B41D6002932AC009C9C9C0000000000D7A14200FAF1E300FAF3E700F7EB
      D500F2DEB800EAC37C00E2AC2F00DFA30100DFA30000DFA30000DFA30000DEA1
      0000D9980300CA8501009C9C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000068A039006BAA4E0077B25E0070AF
      59006DAE560061AA4B0054A640004CA338004BA337004BA337004BA337004AA2
      3400499D24005D962B0000000000000000005159B3004E61C5005E71CC00596C
      C5005669C4005266C4004B61C4004159C5003952C500314AC7002C44C5002B3F
      C2002A39B700464CA9000000000000000000CD973400D69E3C00DAA64D00D8A2
      4400D8A03F00D69A2B00D3941200D2910100D2910000D2910000D2900000D18F
      0000CD890200C58C260000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006331310063313100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009C5A39007B39210063313100633131006331000063313100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000944229007B392100633131006331310063310000633131007B3921000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009C5A39007B39210063310000633131006331000063313100000000000000
      0000000000000000000000000000000000000000000000000000000000009C63
      9C00A5B5F70031319C0031316300310031000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CE6331006331
      630031319C003131CE003131CE003131CE003131CE0031316300313131006331
      3100000000000000000000000000000000000000000000000000CE6331006331
      630031319C003131CE003131CE003131CE003131CE0031319C00313131006331
      31007B3921000000000000000000000000000000000000000000CE6331006331
      630031319C003131CE003131CE003131CE003131CE0031319C00313131006331
      31000000000000000000000000000000000000000000000000009C316300F7F7
      F70063639C0000319C003131CE00313163006331310000000000000000000000
      00000000000000000000000000000000000000000000CE63310063319C003131
      CE00315AE700315AE700315AE700315AE700315AE700315AE7003131CE003131
      63003918100000000000000000000000000000000000CE63310063319C003131
      CE00315AE700315AE700315AE700315AE700315AE700315AE7003131CE003131
      6300633131007B392100000000000000000000000000CE633100633163003131
      CE00315AE700315AE700315AE700315AE700315AE700315AE7003131CE003131
      63006331310000000000000000000000000000000000CE633100639CCE006363
      CE0031319C00315AE700315AE70031319C003918100000000000000000000000
      0000000000000000000000000000000000000000000063319C00315AE700315A
      E700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315AE700315AE7003131
      CE00313163006331310000000000000000000000000063319C00315AE700315A
      E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003131
      CE0031316300633131000000000000000000CE63310063319C00315AE700315A
      E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003131
      CE00313163006331310000000000000000000000000031319C00315AE7003131
      9C003163CE00315AE700315AE7003163CE003131630063313100000000000000
      0000000000000000000000000000000000009C316300315AE700315AE700315A
      E700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315AE700315AE700315A
      E7003131CE003131310000000000000000009C6363003131CE00315AE700315A
      E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
      E7003131CE00313131007B392100000000009C3163003131CE00315AE700315A
      E700A5B5F700CECEFF003163FF00315AE700315AE700315AE7006363FF00315A
      E7003131CE00313131007B3921000000000063316300315AE70031319C003163
      9C00315AE700315AE700315AE7003163FF0031319C0031313100000000000000
      00000000000000000000000000000000000063639C00315AE700315AE700315A
      E700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315AE700315AE700315A
      E700315AE70031319C00633131000000000063639C00315AE700315AE700315A
      E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
      E700315AE70031319C00633131000000000063639C00315AE700315AE700315A
      E7009C9CCE00FFFFFF00A5B5F700315AE700315AE700CECEFF00F7F7F700315A
      E700315AE70031319C006331310000000000315AE7003131CE0031319C00639C
      FF00639CFF00639CFF00639CFF009C9CFF003163CE0031316300633131000000
      000000000000000000000000000000000000315AE700315AE700639CFF006363
      FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639CFF006363FF00639C
      FF00315AE7003131CE0063310000000000003131CE00315AE700A5B5F700A5B5
      F700A5B5F700A5B5F700A5B5F700A5B5F7009C9CFF00A5B5F7009C9CFF00A5B5
      F7006363FF003163CE0063310000000000003131CE00315AE700315AE7003163
      FF003163CE009C9CCE00FFFFFF00A5B5F700F7F7F700CECEFF0031319C00315A
      E700315AE7003131CE006331000000000000639CCE006363CE009C9CCE00639C
      FF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163CE00313131006331
      310000000000000000000000000000000000315AE700315AE700FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00315AE700315AE70063313100000000003163CE00315AE700FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00A5B5F7003131CE0063313100000000003163CE00315AE700639CFF003163
      CE006363FF003163CE00F7F7F700FFFFFF00CECECE003163CE00315AE700315A
      E700315AE7003163CE0063313100000000009C639C00CECECE00A5B5F700CECE
      FF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639CFF0031319C003131
      310000000000000000000000000000000000315AE700315AE700FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00315AE7003131CE007B392100000000003131CE006363FF00CECECE00B5D6
      E700CECECE00B5D6E700CECECE00CECEFF00CECECE00CECEFF00CECECE00CECE
      FF00639CCE003131CE0063313100000000003131CE00315AE7003163FF006363
      FF00639CFF00CEEFF700FFFFFF00FFFFFF00B5D6E7006363FF00315AE700315A
      E700315AE7003131CE007B39210000000000000000009C9C9C009C9CCE009C9C
      CE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEFF700639CFF003131
      630039181000000000000000000000000000315AE7003163FF00A5B5F700A5B5
      F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5F700A5B5F700A5B5
      F700315AE700315AE7007B392100000000003163CE00639CFF003163CE006363
      CE003163CE006363CE006363CE003163CE003163CE003163CE003163CE003163
      CE00315AE7003131CE0094422900000000003163CE00639CFF00639CFF00A5B5
      F700FFFFFF00FFFFFF00A5B5F70031319C00F7F7F700A5B5F700315AE7003163
      FF00315AE7003131CE007B3921000000000000000000000000009C639C009C63
      9C009C3163000000000000000000CE636300CECECE009C9CCE00CECEFF003163
      CE00313163006331310000000000000000006363CE00315AE7006363FF006363
      FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163FF003163CE00315A
      E700315AE70031319C009C5A39000000000063319C006363FF00639CFF00639C
      FF009C9CFF00639CFF00639CFF00639CFF00639CFF006363CE003163FF00315A
      E700315AE70031319C00944229000000000063319C006363FF00639CCE00F7F7
      F700FFFFFF006363CE006363CE00639CFF006363CE00F7F7F700639CFF00315A
      E700315AE70031319C009C5A3900000000000000000000000000000000000000
      000000000000000000000000000000000000CE636300A5B5F7009C9CCE00CECE
      FF0031319C00313131007B39210000000000CE636300315AE700639CFF00639C
      FF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163FF003163FF003163
      FF00315AE700633163000000000000000000CE636300315AE700639CFF00A5B5
      F700A5B5F700A5B5F700A5B5F700639CFF00639CFF00639CFF00639CFF00315A
      E7003163CE00633163000000000000000000CE636300315AE7009C9CFF006363
      CE006363CE00639CCE00A5B5F700639CFF00639CCE006363CE003163CE003163
      FF00315AE7006331630000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CE636300639CCE009C9C
      FF00B5D6E70031319C0094422900000000000000000063639C00315AE700639C
      FF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639CFF006363FF00315A
      E70063319C00CE6331000000000000000000000000009C636300315AE700A5B5
      F7009CCEFF009CCEFF00A5B5F700A5B5F700639CFF00639CFF00639CFF00315A
      E70063319C00CE63310000000000000000000000000063636300315AE700A5B5
      F700A5B5F7009CCEFF00A5B5F700A5B5F700639CFF00639CFF00639CFF00315A
      E700633163000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CE6363006363
      9C006363CE009C9CCE000000000000000000000000000000000063639C00315A
      E700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363CE00315AE7006331
      9C00CE633100000000000000000000000000000000000000000063319C003163
      CE009C9CFF00B5D6E700CECEFF009CCEFF00A5B5F700639CFF003131CE006331
      6300CE633100000000000000000000000000000000000000000063319C003163
      CE009C9CFF00B5D6E700CECEFF009CCEFF00A5B5F700639CFF003131CE006331
      6300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE63
      63006363CE00315AE7003163FF006363FF00315AE7006363CE009C636300CE63
      310000000000000000000000000000000000000000000000000000000000CE63
      630063319C003131CE003163CE003131CE003131CE0063319C009C6363000000
      000000000000000000000000000000000000000000000000000000000000CE63
      630063319C003131CE003163CE003131CE003131CE0063319C009C6363000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000C003C003C00300008001800180010000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00010001000100000003000300030000FFFFFFFFFFFFFFFFF9FFF03FF01FF03F
      E0FFC00FC007C00FC07F800780038007807F800380030003803F000300010001
      003F000100010001001F000100010001000F000100010001000F000100010001
      8007000100010001C603000100010001FF01000300030003FF81800380038007
      FFC3C007C007C00FFFFFE00FE01FE01F00000000000000000000000000000000
      000000000000}
  end
  inherited SKFAskin: TsFrameAdapter
    Left = 644
    Top = 291
  end
end

```
<!-- tabs:end -->

