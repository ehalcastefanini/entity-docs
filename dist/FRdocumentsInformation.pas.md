<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um componente de interface gráfica para gerenciar informações de documentos associados a entidades, como clientes ou consignatários. Ele permite visualizar, adicionar e excluir registros de documentos, além de configurar propriedades específicas para os campos exibidos em uma grade (grid). O objetivo é facilitar a manipulação e visualização de dados relacionados a documentos de forma estruturada e eficiente.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes cxGrid e cxEditRepository:** Utilizados para criar e gerenciar a interface gráfica, incluindo a grade de dados e os editores personalizados.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **ClientDataSet:** Para manipulação de dados em memória.

* **Forma do Componente:**
  - **Grade de Exibição (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `tpDoc` (Tipo de Documento): ComboBox com opções como "INV", "OACK", "PLIST".
      - `nCopies` (Número de Cópias): Campo numérico.
      - `defMethod` (Método de Envio): ComboBox com opções como "MAIL", "FAX", "POSTAL".
      - `addressNum` (Número de Endereço): Campo numérico.
      - `Email`: Campo de texto.
      - `Fax`: Campo de texto.
    - **Ações da Grade e seus Efeitos:**
      - Adicionar (`ADD`): Insere um novo registro.
      - Excluir (`DELETE`): Remove o registro selecionado.

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar um novo documento.
  - Excluir um documento existente.
  - Validar e formatar campos como e-mail, fax e número de endereço.

* **Componentes Principais:**
  - **Grade (Grid):** Exibe os dados dos documentos.
  - **Painel de Ações:** Contém botões para adicionar e excluir registros.
  - **Editores Personalizados:** Configuram a entrada de dados para campos específicos.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `if botão "Adicionar" clicado then insere novo registro`.
  - Evento `OnClick` do botão "Excluir": `if botão "Excluir" clicado then remove registro selecionado`.
  - Evento `OnChange` de um campo: `if valor do campo alterado then valida o campo`.

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização: O componente é carregado e configurado com base no tipo de formulário pai.
  - Interação do Usuário: O usuário pode adicionar ou excluir registros na grade.
  - Funções Executadas:
    - `Create` (Arquivo: `FRdocumentsInformation`): Configura o componente e define propriedades iniciais.
    - `ACTaddExecute` (Arquivo: `FRdocumentsInformation`): Adiciona um novo registro.
    - `CDStableBeforeEdit` e `CDStableBeforePost` (Arquivo: `FRdocumentsInformation`): Validam os dados antes de editar ou salvar.

* **Dados Necessários:**
  - Tipo de Documento.
  - Número de Cópias.
  - Método de Envio.
  - Número de Endereço.
  - E-mail.
  - Fax.

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Adicionar": Disponível sempre.
  - Ação "Excluir": Disponível apenas se um registro estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "E-mail inválido" se o formato do e-mail for incorreto.
  - "Número de endereço inválido" se o valor não atender ao formato esperado.
  - "Fax inválido" se o valor não atender ao formato esperado.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validação de Campos:**
  - `Email`: Deve ter no máximo 300 caracteres.
  - `Fax`: Deve seguir o formato de número de fax.
  - `addressNum` e `nCopies`: Devem ser numéricos.

## 5. Funções Principais:

* **`Create`:** Configura o componente com base no formulário pai e define propriedades iniciais.
* **`ACTaddExecute`:** Adiciona um novo registro à grade.
* **`CDStableBeforeEdit` e `CDStableBeforePost`:** Validam os dados antes de editar ou salvar.

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxEditRepository`: Para a interface gráfica.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base herdada para funcionalidades adicionais.

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `tpDoc` (string, obrigatório, opções: "INV", "OACK", "PLIST").
  - `nCopies` (numérico, obrigatório, máximo: 2 dígitos).
  - `defMethod` (string, obrigatório, opções: "MAIL", "FAX", "POSTAL").
  - `addressNum` (numérico, obrigatório, máximo: 2 dígitos).
  - `Email` (string, opcional, máximo: 300 caracteres).
  - `Fax` (string, opcional, formato de fax).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  FRAMEdocumentsInformation := TFRAMEdocumentsInformation.Create(Self);
  FRAMEdocumentsInformation.ShowActionPanel := True;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>Tipo de Documento</th>
        <th>Número de Cópias</th>
        <th>Método de Envio</th>
        <th>Número de Endereço</th>
        <th>Email</th>
        <th>Fax</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>INV</td>
        <td>1</td>
        <td>MAIL</td>
        <td>12</td>
        <td>example@example.com</td>
        <td>+123456789</td>
      </tr>
    </tbody>
  </table>
  ```

## 11. Comentários Importantes no Código:

* Configuração de propriedades específicas para diferentes tipos de formulários no método `Create`.
* Validação de campos no método `CDStableBeforePost`.

## 12. Conclusão:

O código implementa uma interface robusta para gerenciar informações de documentos, com validações e configurações específicas para os campos. No entanto, faltam detalhes sobre integração com APIs externas e valores padrão para alguns campos.

## 13. Resumo Curto:

Componente Delphi para gerenciar informações de documentos em uma grade, com validações e editores personalizados. Suporta ações de adicionar e excluir registros, configurando propriedades específicas para diferentes tipos de formulários.#### **FRdocumentsInformation.pas**

```
unit FRdocumentsInformation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  cxImageComboBox;

type
  TFRAMEdocumentsInformation = class(TFRAMEBaseGridEditSOA)
    cxICBdefMethd: TcxEditRepositoryImageComboBoxItem;
    cxEDTaddressNum: TcxEditRepositoryMaskItem;
    cxEDTFax: TcxEditRepositoryMaskItem;
    cxICBtypeDoc: TcxEditRepositoryImageComboBoxItem;
    cxEDTnCopies: TcxEditRepositoryMaskItem;
    cxEDTemail: TcxEditRepositoryMaskItem;
    procedure CDStableBeforeEdit(DataSet: TDataSet);
    procedure CDStableBeforePost(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);

  private
    { Private declarations }
    FdefaultsLoaded: Boolean;
    function m_ExistsRecord(const pv_dataset: TClientDataSet;
      pv_fieldNames: string): Boolean;
    function ValidEmails(pv_Unique: Boolean = false): Boolean;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEdocumentsInformation: TFRAMEdocumentsInformation;

implementation

uses
  kneUtils, kneTypes, kneFGDBUtils, Global, kneFREditSOA;

{$R *.dfm}


constructor TFRAMEdocumentsInformation.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  if Tform(owner).ClassName = 'TFORMMconsignee' then
    MasterKeyFields := 'consCode=entity;entityType'
  else
  if Tform(owner).ClassName = 'TFORMEcustomerAddressDoc' then
    MasterKeyFields := 'customer=entity'
  else             // TFORMMcustomer
    MasterKeyFields := 'customerCode=entity;entityType';
    
  DataPacketName := 'EntityDocs';
  PropertyName := 'docs';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';
    
  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('tpDoc; nCopies; defMethod; addressNum; Email; Fax; ' +
    	' name; addText; lastUpd; updBy'); // 07-06-2011, # 8943 novos campos
    // Key Fields ..............................................................
    KeyFields:= 'tpDoc;defMethod';
    // Custom Editors ..........................................................
    AddCustomField('tpDoc', 'cxICBtypeDoc');
    AddCustomField('defMethod', 'cxICBdefMethd');
    AddCustomField('addressNum', 'cxEDTaddressNum');
    AddCustomField('nCopies', 'cxEDTnCopies');
    AddCustomField('Email', 'cxEDTemail');
    AddCustomField('Fax', 'cxEDTFax');
  end; //with
  ColsWidthInGrid := '80;75;90;80;450;100;200;150;100;80'; // 07-06-2011, # 8943 novos campos
  
  FdefaultsLoaded := False;
//  UseColsBestFit := False;

  m_GetTypeDoc(cxICBtypeDoc);        //JAR #8593       04-02-2011
  m_GetConfType(cxICBdefMethd);      //JAR #9421/9422  29-04-2011    
  CDStable.Tag := 10;
  DStable.Tag := 10;

```

#### **FRdocumentsInformation.dfm**

```
inherited FRAMEdocumentsInformation: TFRAMEdocumentsInformation
  ParentFont = True
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxICBdefMethd: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'MAIL'
          ImageIndex = 0
          Value = 'MAIL'
        end
        item
          Description = 'FAX'
          Value = 'FAX'
        end
        item
          Description = 'POSTAL'
          Value = 'POSTAL'
        end>
    end
    object cxEDTaddressNum: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{1,2}'
    end
    object cxEDTFax: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '(\+\d{1,4})?\d{1,15}'
    end
    object cxICBtypeDoc: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'INV'
          ImageIndex = 0
          Value = 'INV'
        end
        item
          Description = 'OACK'
          Value = 'OACK'
        end
        item
          Description = 'PLIST'
          Value = 'PLIST'
        end>
    end
    object cxEDTnCopies: TcxEditRepositoryMaskItem
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{1,2}'
    end
    object cxEDTemail: TcxEditRepositoryMaskItem
      Properties.MaxLength = 300
    end
  end
end
```
<!-- tabs:end -->

